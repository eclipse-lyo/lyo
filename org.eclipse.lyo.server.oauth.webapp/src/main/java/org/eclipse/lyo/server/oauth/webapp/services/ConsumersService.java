/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
 *
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the Eclipse Public License v1.0
 *  and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *  
 *  The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 *  and the Eclipse Distribution License is available at
 *  http://www.eclipse.org/org/documents/edl-v10.php.
 *  
 *  Contributors:
 *  
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.server.oauth.webapp.services;

import java.util.Collection;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import net.oauth.OAuthProblemException;

import org.apache.wink.json4j.JSONArray;
import org.apache.wink.json4j.JSONException;
import org.apache.wink.json4j.JSONObject;
import org.eclipse.lyo.server.oauth.core.OAuthConfiguration;
import org.eclipse.lyo.server.oauth.core.consumer.ConsumerStore;
import org.eclipse.lyo.server.oauth.core.consumer.ConsumerStoreException;
import org.eclipse.lyo.server.oauth.core.consumer.LyoOAuthConsumer;

/**
 * Manages OAuth consumers for this provider.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 */
@Path("/oauth/consumers")
public class ConsumersService {
	
	@Context protected HttpServletRequest httpRequest;
	@Context protected HttpServletResponse httpResponse;

	@GET
	@Produces({ MediaType.APPLICATION_JSON })
	public Response getAllConsumers() throws JSONException {
		try {
			if (!OAuthConfiguration.getInstance().getApplication()
					.isAdminSession(httpRequest)) {
				return Response.status(Status.FORBIDDEN)
						.type(MediaType.TEXT_PLAIN)
						.entity("You must be an administrator.").build();
			}

			Collection<LyoOAuthConsumer> consumers = OAuthConfiguration
					.getInstance().getConsumerStore().getAllConsumers();
			JSONArray provisionalConsumers = new JSONArray();
			JSONArray approvedConsumers = new JSONArray();

			for (LyoOAuthConsumer consumer : consumers) {
				if (consumer.isProvisional()) {
					provisionalConsumers.add(asJson(consumer));
				} else {
					approvedConsumers.add(asJson(consumer));
				}
			}

			JSONObject response = new JSONObject();
			response.put("provisional", provisionalConsumers);
			response.put("approved", approvedConsumers);

			return Response.ok(response.write()).type(MediaType.APPLICATION_JSON)
					.build();
		} catch (ConsumerStoreException e) {
			return handleConsumerStoreException(e);
		} catch (OAuthProblemException e) {
			return Response.status(Status.SERVICE_UNAVAILABLE).build();
		}
	}
	
	/**
	 * Updates an OAuth consumer.
	 * 
	 * @param key
	 *            the consumer key
	 * @param name
	 *            the new name or null
	 * @param trusted
	 *            "true" the consumer is trusted. Can be null.
	 * @param provisional
	 *            "true" if the consumer is provisional or "false" if the
	 *            consumer is authorized. Can be null.
	 * @return the HTTP response
	 */
	@POST
	@Path("/{key}")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	public Response updateConsumer(@PathParam("key") String key,
			@FormParam("name") String name,
			@FormParam("trusted") String trusted,
			@FormParam("provisional") String provisional) {
		try {
			if (!OAuthConfiguration.getInstance().getApplication()
					.isAdminSession(httpRequest)) {
				return Response.status(Status.FORBIDDEN)
						.type(MediaType.TEXT_PLAIN)
						.entity("You must be an administrator.").build();
			}
			
			ConsumerStore store = OAuthConfiguration.getInstance()
					.getConsumerStore();
			LyoOAuthConsumer consumer = store.getConsumer(key);
			if (consumer == null) {
				return Response.status(Status.NOT_FOUND).build();
			}
			if (name != null) {
				consumer.setName(name);
			}
			if (trusted != null) {
				consumer.setTrusted("true".equals(trusted));
			}
			if (provisional != null) {
				consumer.setProvisional("true".equals(provisional));
			}
			store.updateConsumer(consumer);
			
			return Response.noContent().build();
		} catch (ConsumerStoreException e) {
			return handleConsumerStoreException(e);
		} catch (OAuthProblemException e) {
			return Response.status(Status.SERVICE_UNAVAILABLE).build();
		}
	}

	/**
	 * Deletes an OAuth consumer.
	 * 
	 * @param key
	 *            the consumer key
	 * @return the HTTP response
	 */
	@DELETE
	@Path("/{key}")
	public Response removeConsumer(@PathParam("key") String key) {
		try {
			if (!OAuthConfiguration.getInstance().getApplication()
					.isAdminSession(httpRequest)) {
				return Response.status(Status.FORBIDDEN)
						.type(MediaType.TEXT_PLAIN)
						.entity("You must be an administrator.").build();
			}
			
			OAuthConfiguration.getInstance().getConsumerStore()
					.removeConsumer(key);
			return Response.noContent().build();
		} catch (ConsumerStoreException e) {
			return handleConsumerStoreException(e);
		} catch (OAuthProblemException e) {
			return Response.status(Status.SERVICE_UNAVAILABLE).build();
		}
	}

	protected Response handleConsumerStoreException(ConsumerStoreException e) {
		e.printStackTrace();
		return Response.status(Status.CONFLICT).type(MediaType.TEXT_PLAIN)
				.entity(e.getMessage()).build();
	}

	protected JSONObject asJson(LyoOAuthConsumer consumer) throws JSONException {
		JSONObject o = new JSONObject();
		o.put("name", consumer.getName());
		o.put("key", consumer.consumerKey);
		o.put("provisional", consumer.isProvisional());
		o.put("trusted", consumer.isTrusted());

		return o;
	}
}
