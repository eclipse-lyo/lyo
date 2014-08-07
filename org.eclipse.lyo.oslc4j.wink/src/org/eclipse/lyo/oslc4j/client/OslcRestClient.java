/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *	
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *
 *	   Russell Boykin		- initial API and implementation
 *	   Alberto Giammaria	- initial API and implementation
 *	   Chris Peters			- initial API and implementation
 *	   Gianluca Bernardini	- initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.client;

import java.net.URI;
import java.util.Collection;
import java.util.Set;

import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.core.Application;

import org.apache.wink.client.ClientConfig;
import org.apache.wink.client.ClientResponse;
import org.apache.wink.client.ClientWebException;
import org.apache.wink.client.EntityType;
import org.apache.wink.client.Resource;
import org.apache.wink.client.RestClient;
import org.apache.wink.client.handlers.ClientHandler;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;

public final class OslcRestClient
{
	public static final int DEFAULT_READ_TIMEOUT = 60000;

	private final Set<Class<?>> providers;
	private final String		uri;
	private final Resource		clientResource;
	private final String		mediaType;
	private final int			readTimeout;

	public OslcRestClient(final Set<Class<?>> providers,
						  final String		  uri,
						  final String		  mediaType,
						  final int			  readTimeout,
						  final ClientHandler ... handlers)
	{
		super();

		this.providers	 = providers;
		this.uri		 = uri;
		this.mediaType	 = mediaType;
		this.readTimeout = readTimeout;

		final ClientConfig clientConfig = new ClientConfig();

		if ((providers != null) &&
			(providers.size() > 0))
		{
			final Application application = new Application()
			{
				@Override
				public Set<Class<?>> getClasses()
				{
					return providers;
				}
			};

			// We have to ensure that our providers are registered since when running in a non-server environment, they are not.
			clientConfig.applications(application);
		}

		clientConfig.readTimeout(readTimeout);
		
		if ((handlers != null) && 
			(handlers.length > 0))
		{
			clientConfig.handlers(handlers);
		}

		final RestClient client = new RestClient(clientConfig);

		this.clientResource = client.resource(uri);
	}

	public OslcRestClient(final Set<Class<?>> providers,
						  final String		  uri,
						  final String		  mediaType)
	{
		this(providers,
			 uri,
			 mediaType,
			 DEFAULT_READ_TIMEOUT);
	}

	public OslcRestClient(final Set<Class<?>> providers,
						  final String		  uri,
						  final int			  timeout)
	{
		this(providers,
			 uri,
			 OslcMediaType.APPLICATION_RDF_XML,
			 timeout);
	}

	public OslcRestClient(final Set<Class<?>> providers,
						  final String		  uri)
	{
		this(providers,
			 uri,
			 OslcMediaType.APPLICATION_RDF_XML,
			 DEFAULT_READ_TIMEOUT);
	}

	public OslcRestClient(final Set<Class<?>> providers,
						  final URI			  uri,
						  final String		  mediaType,
						  final int			  readTimeout)
	{
		this(providers,
			 uri.toString(),
			 mediaType,
			 readTimeout);
	}

	public OslcRestClient(final Set<Class<?>> providers,
						  final URI			  uri,
						  final String		  mediaType)
	{
		this(providers,
			 uri.toString(),
			 mediaType,
			 DEFAULT_READ_TIMEOUT);
	}

	public OslcRestClient(final Set<Class<?>> providers,
						  final URI			  uri,
						  final int timeout)
	{
		this(providers,
			 uri.toString(),
			 OslcMediaType.APPLICATION_RDF_XML,
			 timeout);
	}

	public OslcRestClient(final Set<Class<?>> providers,
						  final URI			  uri)
	{
		this(providers,
			 uri.toString(),
			 OslcMediaType.APPLICATION_RDF_XML,
			 DEFAULT_READ_TIMEOUT);
	}

	public Set<Class<?>> getProviders()
	{
		return providers;
	}

	public String getMediaType()
	{
		return mediaType;
	}

	public int getReadTimeout()
	{
		return readTimeout;
	}

	public String getUri()
	{
		return uri;
	}

	public Resource getClientResource()
	{
		return clientResource;
	}

	public <T> T getOslcResource(final Class<T> oslcResourceClass)
	{
		try
		{
			final ClientResponse response = clientResource.accept(mediaType).get();

			final int statusCode = response.getStatusCode();

			if (HttpServletResponse.SC_OK == statusCode)
			{
				return response.getEntity(oslcResourceClass);
			}

			throw new ClientWebException(null, response);
		}
		catch (final ClientWebException exception)
		{
			final ClientResponse response = exception.getResponse();

			if (response != null)
			{
				final int statusCode = response.getStatusCode();

				if ((HttpServletResponse.SC_NO_CONTENT == statusCode) ||
					(HttpServletResponse.SC_NOT_FOUND  == statusCode) ||
					(HttpServletResponse.SC_GONE	   == statusCode))
				{
					return null;
				}
			}

			throw exception;
		}
	}

	public <T> T[] getOslcResources(final Class<T[]> oslcResourceArrayClass)
	{
		try
		{
			final ClientResponse response = clientResource.accept(mediaType).get();

			final int statusCode = response.getStatusCode();

			if (HttpServletResponse.SC_OK == statusCode)
			{
				return response.getEntity(oslcResourceArrayClass);
			}

			throw new ClientWebException(null, response);
		}
		catch (final ClientWebException exception)
		{
			final ClientResponse response = exception.getResponse();

			if (response != null)
			{
				final int statusCode = response.getStatusCode();

				if ((HttpServletResponse.SC_NO_CONTENT == statusCode) ||
					(HttpServletResponse.SC_NOT_FOUND  == statusCode) ||
					(HttpServletResponse.SC_GONE	   == statusCode))
				{
					return null;
				}
			}

			throw exception;
		}
	}

	public <T extends Collection<?>> T getOslcResources(final EntityType<T> entityType)
	{
		try
		{
			final ClientResponse response = clientResource.accept(mediaType).get();

			final int statusCode = response.getStatusCode();

			if (HttpServletResponse.SC_OK == statusCode)
			{
				return response.getEntity(entityType);
			}

			throw new ClientWebException(null, response);
		}
		catch (final ClientWebException exception)
		{
			final ClientResponse response = exception.getResponse();

			if (response != null)
			{
				final int statusCode = response.getStatusCode();

				if ((HttpServletResponse.SC_NO_CONTENT == statusCode) ||
					(HttpServletResponse.SC_NOT_FOUND  == statusCode) ||
					(HttpServletResponse.SC_GONE	   == statusCode))
				{
					return null;
				}
			}

			throw exception;
		}
	}

	@SuppressWarnings("unchecked")
	public <T> T addOslcResource(final T oslcResource)
	{
		final ClientResponse response = clientResource.contentType(mediaType).accept("*/*").post(ClientResponse.class, oslcResource);

		final int statusCode = response.getStatusCode();

		if ((HttpServletResponse.SC_OK		== statusCode) ||
			(HttpServletResponse.SC_CREATED == statusCode))
		{
			return (T) response.getEntity(oslcResource.getClass());
		}

		throw new ClientWebException(null, response);
	}

	public ClientResponse addOslcResourceReturnClientResponse(final Object oslcResource)
	{
		try
		{
			return clientResource.contentType(mediaType).accept("*/*").post(ClientResponse.class, oslcResource);
		}
		catch (final ClientWebException exception)
		{
			final ClientResponse response = exception.getResponse();

			if (response != null)
			{
				return response;
			}

			throw exception;
		}
	}

	public ClientResponse updateOslcResourceReturnClientResponse(final Object oslcResource)
	{
		try
		{
			return clientResource.contentType(mediaType).put(ClientResponse.class, oslcResource);
		}
		catch (final ClientWebException exception)
		{
			final ClientResponse response = exception.getResponse();

			if (response != null)
			{
				return response;
			}

			throw exception;
		}
	}

	public ClientResponse removeOslcResourceReturnClientResponse()
	{
		try
		{
			return clientResource.accept("*/*").delete();
		}
		catch (final ClientWebException exception)
		{
			final ClientResponse response = exception.getResponse();

			if (response != null)
			{
				return response;
			}

			throw exception;
		}
	}
}
