/*******************************************************************************
 * Copyright (c) 2012, 2013 IBM Corporation.
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
package org.eclipse.lyo.oslc4j.provider.json4j;

import java.io.InputStream;
import java.io.OutputStream;
import java.lang.annotation.Annotation;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.ResponseBuilder;
import javax.ws.rs.ext.Providers;

import org.apache.wink.json4j.JSONObject;
import org.eclipse.lyo.oslc4j.core.OSLC4JConstants;
import org.eclipse.lyo.oslc4j.core.OSLC4JUtils;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.exception.MessageExtractor;
import org.eclipse.lyo.oslc4j.core.model.Error;
import org.eclipse.lyo.oslc4j.core.model.ResponseInfo;
import org.eclipse.lyo.oslc4j.core.model.ResponseInfoArray;

/**
 * Use JSON-LD support in Jena provider.
 */
@Deprecated
public abstract class AbstractOslcRdfJsonProvider
{
	private static final Logger logger = Logger.getLogger(AbstractOslcRdfJsonProvider.class.getName());

	private static final Annotation[] ANNOTATIONS_EMPTY_ARRAY = new Annotation[0];
	private static final Class<Error> CLASS_OSLC_ERROR		  = Error.class;

	private @Context HttpHeaders		  httpHeaders;		  // Available only on the server
	protected @Context HttpServletRequest httpServletRequest; // Available only on the server
	private @Context Providers			  providers;		  // Available on both client and server

	protected AbstractOslcRdfJsonProvider()
	{
		super();
	}

	protected static boolean isWriteable(final Class<?>		type,
										 final Annotation[] annotations,
										 final MediaType	requiredMediaType,
										 final MediaType	actualMediaType)
	{
		if (type.getAnnotation(OslcResourceShape.class) != null)
		{
			// When handling "recursive" writing of an OSLC Error object, we get a zero-length array of annotations
			if ((annotations != null) &&
				((annotations.length > 0) ||
				 (Error.class != type)))
			{
				for (final Annotation annotation : annotations)
				{
					if (annotation instanceof Produces)
					{
						final Produces producesAnnotation = (Produces) annotation;

						for (final String value : producesAnnotation.value())
						{
							if (requiredMediaType.isCompatible(MediaType.valueOf(value)))
							{
								return true;
							}
						}
					}
				}

				return false;
			}

			// We do not have annotations when running from the non-web client.
			return requiredMediaType.isCompatible(actualMediaType);
		}

		return false;
	}

	protected void writeTo(final boolean						queryResult,
						   final Object[]						objects,
						   final MediaType						errorMediaType,
						   final MultivaluedMap<String, Object> map,
						   final OutputStream					outputStream)
			  throws WebApplicationException
	{
		boolean isClientSide = false;

		try {
			httpServletRequest.getMethod();
		} catch (RuntimeException e) {
			isClientSide = true;
		}

		String descriptionURI  = null;
		String responseInfoURI = null;

		if (queryResult && ! isClientSide)
		{

			final String method = httpServletRequest.getMethod();
			if ("GET".equals(method))
			{
				descriptionURI =  OSLC4JUtils.resolveURI(httpServletRequest,true);
				responseInfoURI = descriptionURI;

				final String queryString = httpServletRequest.getQueryString();
				if ((queryString != null) &&
					(isOslcQuery(queryString)))
				{
					responseInfoURI += "?" + queryString;
				}
			}

		}

		final JSONObject jsonObject;

		@SuppressWarnings("unchecked")
		final Map<String, Object> properties = isClientSide ?
			null :
			(Map<String, Object>)httpServletRequest.getAttribute(OSLC4JConstants.OSLC4J_SELECTED_PROPERTIES);
		final String nextPageURI = isClientSide ?
				null :
				(String)httpServletRequest.getAttribute(OSLC4JConstants.OSLC4J_NEXT_PAGE);
		final Integer totalCount = isClientSide ?
				null :
				(Integer)httpServletRequest.getAttribute(OSLC4JConstants.OSLC4J_TOTAL_COUNT);

		ResponseInfo<?> responseInfo = new ResponseInfoArray<Object>(null, properties, totalCount, nextPageURI);

		try
		{
			jsonObject = JsonHelper.createJSON(descriptionURI,
											   responseInfoURI,
											   responseInfo,
											   objects,
											   properties);

			jsonObject.write(outputStream,
							 true);
		}
		catch (final Exception exception)
		{
			logger.log(Level.FINE, MessageExtractor.getMessage("ErrorSerializingResource"), exception);
			throw new WebApplicationException(exception);
		}
	}

	protected void writeTo(final Object[]						objects,
						   final MediaType						errorMediaType,
						   final MultivaluedMap<String, Object> map,
						   final OutputStream					outputStream,
						   final Map<String, Object>			properties,
						   final String							descriptionURI,
						   final String							responseInfoURI,
						   final ResponseInfo<?>					responseInfo)
				throws WebApplicationException
	{
		final JSONObject jsonObject;

		try
		{
			jsonObject = JsonHelper.createJSON(descriptionURI,
											   responseInfoURI,
											   responseInfo,
											   objects,
											   properties);

			jsonObject.write(outputStream, true);
		}
		catch (final Exception exception)
		{
			logger.log(Level.FINE, MessageExtractor.getMessage("ErrorSerializingResource"), exception);
			throw new WebApplicationException(exception);
		}
	}

	 protected static boolean isReadable(final Class<?>	 type,
										final MediaType requiredMediaType,
										final MediaType actualMediaType)
	{
		return (type.getAnnotation(OslcResourceShape.class) != null) &&
			   (requiredMediaType.isCompatible(actualMediaType));
	}

	protected Object[] readFrom(final Class<?>						 type,
								final MediaType						 errorMediaType,
								final MultivaluedMap<String, String> map,
								final InputStream					 inputStream)
		   throws WebApplicationException
	{
		try
		{
			final JSONObject jsonObject = new JSONObject(inputStream);

			return JsonHelper.fromJSON(jsonObject,
									   type);
		}
		catch (final Exception exception)
		{
			throw new WebApplicationException(exception,
											  buildBadRequestResponse(exception,
																	  errorMediaType,
																	  map));
		}
	}

	protected Response buildBadRequestResponse(final Exception				   exception,
											   final MediaType				   errorMediaType,
											   final MultivaluedMap<String, ?> map)
	{
		final MediaType determinedErrorMediaType = determineErrorMediaType(errorMediaType,
																		   map);

		final Error error = new Error();

		error.setStatusCode(String.valueOf(Response.Status.BAD_REQUEST.getStatusCode()));
		error.setMessage(exception.getMessage());

		final ResponseBuilder responseBuilder = Response.status(Response.Status.BAD_REQUEST);
		return responseBuilder.type(determinedErrorMediaType).entity(error).build();
	}

	/**
	 * We handle the case where a client requests an accept type different than their content type.
	 * This will work correctly in the successful case.	 But, in the error case where our
	 * MessageBodyReader/MessageBodyWriter fails internally, we respect the client's requested accept type.
	 */
	private MediaType determineErrorMediaType(final MediaType				  initialErrorMediaType,
											  final MultivaluedMap<String, ?> map)
	{
		try
		{
			// HttpHeaders will not be available on the client side and will throw a NullPointerException
			final List<MediaType> acceptableMediaTypes = httpHeaders.getAcceptableMediaTypes();

			// Since we got here, we know we are executing on the server

			if (acceptableMediaTypes != null)
			{
				for (final MediaType acceptableMediaType : acceptableMediaTypes)
				{
					// If a concrete media type with a MessageBodyWriter
					if (isAcceptableMediaType(acceptableMediaType))
					{
						return acceptableMediaType;
					}
				}
			}
		}
		catch (final NullPointerException exception)
		{
			// Ignore NullPointerException since this means the context has not been set since we are on the client

			// See if the MultivaluedMap for headers is available
			if (map != null)
			{
				final Object acceptObject = map.getFirst("Accept");

				if (acceptObject instanceof String)
				{
					final String[] accepts = acceptObject.toString().split(",");

					double	  winningQ		   = 0.0D;
					MediaType winningMediaType = null;

					for (final String accept : accepts)
					{
						final MediaType acceptMediaType = MediaType.valueOf(accept);

						// If a concrete media type with a MessageBodyWriter
						if (isAcceptableMediaType(acceptMediaType))
						{
							final String stringQ = acceptMediaType.getParameters().get("q");

							final double q = stringQ == null ? 1.0D : Double.parseDouble(stringQ);

							// Make sure and exclude blackballed media type
							if (Double.compare(q, 0.0D) > 0)
							{
								if ((winningMediaType == null) ||
									(Double.compare(q, winningQ) > 0))
								{
									winningMediaType = acceptMediaType;
									winningQ		 = q;
								}
							}
						}
					}

					if (winningMediaType != null)
					{
						return winningMediaType;
					}
				}
			}
		}

		return initialErrorMediaType;
	}

	/**
	 * Only allow media types that are not wildcards and have a related MessageBodyWriter for OSLC Error.
	 */
	private boolean isAcceptableMediaType(final MediaType mediaType)
	{
		return (!mediaType.isWildcardType()) &&
			   (!mediaType.isWildcardSubtype()) &&
			   (providers.getMessageBodyWriter(CLASS_OSLC_ERROR,
											   CLASS_OSLC_ERROR,
											   ANNOTATIONS_EMPTY_ARRAY,
											   mediaType) != null);
	}
	protected static boolean isOslcQuery(final String parmString)
	{
		boolean containsOslcParm = false;

		final String [] uriParts = parmString.toLowerCase().split("oslc\\.",2);
		if (uriParts.length > 1)
		{
			containsOslcParm = true;
		}

		return containsOslcParm;
	}
}
