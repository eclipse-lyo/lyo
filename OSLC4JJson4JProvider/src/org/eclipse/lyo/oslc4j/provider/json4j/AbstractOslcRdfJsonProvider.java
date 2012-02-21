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
 *     Russell Boykin       - initial API and implementation
 *     Alberto Giammaria    - initial API and implementation
 *     Chris Peters         - initial API and implementation
 *     Gianluca Bernardini  - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.provider.json4j;

import java.io.InputStream;
import java.io.OutputStream;
import java.lang.annotation.Annotation;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.ResponseBuilder;

import org.apache.wink.json4j.JSONObject;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.model.Error;

public abstract class AbstractOslcRdfJsonProvider
{
    private @Context HttpServletRequest httpServletRequest;

    protected AbstractOslcRdfJsonProvider()
    {
        super();
    }

    protected static boolean isWriteable(final Class<?>     type,
                                         final Annotation[] annotations,
                                         final MediaType    requiredMediaType,
                                         final MediaType    actualMediaType)
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

    protected void writeTo(final boolean      queryResult,
                           final Object[]     objects,
                           final MediaType    errorMediaType,
                           final OutputStream outputStream)
              throws WebApplicationException
    {
        String descriptionURI  = null;
        String responseInfoURI = null;

        if (queryResult)
        {
            try
            {
                final String method = httpServletRequest.getMethod();
                if ("GET".equals(method))
                {
                    final String scheme      = httpServletRequest.getScheme();
                    final String serverName  = httpServletRequest.getServerName();
                    final int    serverPort  = httpServletRequest.getServerPort();
                    final String contextPath = httpServletRequest.getContextPath();
                    final String pathInfo    = httpServletRequest.getPathInfo();
                    final String queryString = httpServletRequest.getQueryString();

                    descriptionURI = scheme + "://" + serverName + ":" + serverPort + contextPath;

                    if (pathInfo != null)
                    {
                        descriptionURI += pathInfo;
                    }

                    responseInfoURI = descriptionURI;

                    if ((queryString != null) && isOslcQuery(queryString))
                    {
                        responseInfoURI += "?" + queryString;
                    }
                }
            }
            catch (final NullPointerException exception)
            {
                // Ignore since this means the context has not been set
            }
        }

        final JSONObject jsonObject;

        try
        {
            jsonObject = JsonHelper.createJSON(descriptionURI,
                                               responseInfoURI,
                                               objects);

            jsonObject.write(outputStream,
                             true);
        }
        catch (final Exception exception)
        {
            throw new WebApplicationException(exception,
                                              buildBadRequestResponse(exception,
                                                                      errorMediaType));
        }
    }

    protected static boolean isReadable(final Class<?>  type,
                                        final MediaType requiredMediaType,
                                        final MediaType actualMediaType)
    {
        return (type.getAnnotation(OslcResourceShape.class) != null) &&
               (requiredMediaType.isCompatible(actualMediaType));
    }

    protected static Object[] readFrom(final Class<?>    type,
                                       final MediaType   errorMediaType,
                                       final InputStream inputStream)
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
                                                                      errorMediaType));
        }
    }

    protected static Response buildBadRequestResponse(final Exception exception,
                                                      final MediaType errorMediaType)
    {
        final Error error = new Error();

        error.setStatusCode(String.valueOf(Response.Status.BAD_REQUEST.getStatusCode()));
        error.setMessage(exception.getMessage());

        final ResponseBuilder responseBuilder = Response.status(Response.Status.BAD_REQUEST);
        return responseBuilder.type(errorMediaType).entity(error).build();
    }
    
	public static boolean isOslcQuery (String parmString)
	{
		boolean containsOslcParm = false;

		String [] uriParts = parmString.toLowerCase().split("oslc\\.",2);
		if (uriParts.length > 1)
		{
			containsOslcParm = true;
		}
		
		return containsOslcParm;
	}
}