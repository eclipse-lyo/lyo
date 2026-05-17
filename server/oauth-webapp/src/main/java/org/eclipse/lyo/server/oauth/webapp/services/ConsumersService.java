/*
 * Copyright (c) 2022 Contributors to the Eclipse Foundation
 *
 * See the NOTICE file(s) distributed with this work for additional
 * information regarding copyright ownership.
 *
 * This program and the accompanying materials are made available under the
 * terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0, or the Eclipse Distribution License 1.0
 * which is available at http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Clause
 */

package org.eclipse.lyo.server.oauth.webapp.services;

import java.util.Collection;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.eclipse.lyo.oslc4j.provider.json4j.internal.JSONArray;
import org.eclipse.lyo.oslc4j.provider.json4j.internal.JSONObject;
import org.eclipse.lyo.oslc4j.provider.json4j.internal.JSONException;
import org.eclipse.lyo.server.oauth.core.OAuthConfiguration;
import org.eclipse.lyo.server.oauth.core.consumer.ConsumerStore;
import org.eclipse.lyo.server.oauth.core.consumer.ConsumerStoreException;
import org.eclipse.lyo.server.oauth.core.consumer.LyoOAuthConsumer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.DELETE;
import jakarta.ws.rs.FormParam;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.PathParam;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.Response.Status;
import net.oauth.OAuthProblemException;

/**
 * Manages OAuth consumers for this provider.
 *
 * @author Samuel Padgett
 */
@Path("/oauth/consumers")
public class ConsumersService {
    private static final Logger log = LoggerFactory.getLogger(ConsumersService.class);

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

			return Response
			        .ok(response.write())
			        .type(MediaType.APPLICATION_JSON)
			        .header(OAuthServerConstants.HDR_CACHE_CONTROL,
			                OAuthServerConstants.NO_CACHE).build();
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
		CSRFPrevent.check(httpRequest);

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
		CSRFPrevent.check(httpRequest);

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
		log.warn("Exception caught from consumer store: {}", e.getMessage());
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
