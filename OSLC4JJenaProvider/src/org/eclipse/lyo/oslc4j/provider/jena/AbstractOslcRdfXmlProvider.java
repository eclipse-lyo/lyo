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
package org.eclipse.lyo.oslc4j.provider.jena;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.lang.annotation.Annotation;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.ResponseBuilder;

import org.eclipse.lyo.oslc4j.core.OSLC4JUtils;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.model.Error;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.RDFReader;
import com.hp.hpl.jena.rdf.model.RDFWriter;
import com.hp.hpl.jena.util.FileUtils;

public abstract class AbstractOslcRdfXmlProvider
{
    private static final ErrorHandler ERROR_HANDLER = new ErrorHandler();

    private @Context HttpServletRequest httpServletRequest;

    protected AbstractOslcRdfXmlProvider()
    {
        super();
    }

    protected static boolean isWriteable(final Class<?>      type,
                                         final Annotation[]  annotations,
                                         final MediaType     actualMediaType,
                                         final MediaType ... requiredMediaTypes)
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
                            for (final MediaType requiredMediaType : requiredMediaTypes)
                            {
                                if (requiredMediaType.isCompatible(MediaType.valueOf(value)))
                                {
                                    return true;
                                }
                            }
                        }

                        return false;
                    }
                }

                return false;
            }

            // We do not have annotations when running from the non-web client.
            for (final MediaType requiredMediaType : requiredMediaTypes)
            {
                if (requiredMediaType.isCompatible(actualMediaType))
                {
                    return true;
                }
            }
        }

        return false;
    }

    protected void writeTo(final boolean      queryResult,
                           final Object[]     objects,
                           final MediaType    baseMediaType,
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
                	final String publicURI   = OSLC4JUtils.getPublicURI();
                	
                	if (publicURI != null)
                	{
                		descriptionURI = publicURI;
                	}
                	else
                	{
                		final String scheme      = httpServletRequest.getScheme();
                		final String serverName  = httpServletRequest.getServerName();
                		final int    serverPort  = httpServletRequest.getServerPort();
                		final String contextPath = httpServletRequest.getContextPath();
                		
                		descriptionURI = scheme + "://" + serverName + ":" + serverPort + contextPath;
                	}
                	
                    final String pathInfo    = httpServletRequest.getPathInfo();
                    final String queryString = httpServletRequest.getQueryString();

                    if (pathInfo != null)
                    {
                        descriptionURI += pathInfo;
                    }

                    responseInfoURI = descriptionURI;

                    if ((queryString != null) &&
                        (isOslcQuery(queryString)))
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

        // Determine whether to serialize in xml or abreviated xml based upon mediatype.
        // application/rdf+xml yields xml
        // applicaton/xml yields abbreviated xml
        final String serializationLanguage;
        if (OslcMediaType.APPLICATION_RDF_XML_TYPE.isCompatible(baseMediaType))
        {
            serializationLanguage = FileUtils.langXML;
        }
        else
        {
            serializationLanguage = FileUtils.langXMLAbbrev;
        }

        try
        {
            final Model model = JenaModelHelper.createJenaModel(descriptionURI,
                                                                responseInfoURI,
                                                                objects);

            final RDFWriter writer = model.getWriter(serializationLanguage);
            writer.setProperty("showXmlDeclaration",
                    "false");
            writer.setErrorHandler(ERROR_HANDLER);
            
            String xmlDeclaration = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
            outputStream.write(xmlDeclaration.getBytes());
                     
            writer.write(model,
                    outputStream,
                    null);
        }
        catch (final Exception exception)
        {
            throw new WebApplicationException(exception,
                                              buildBadRequestResponse(exception,
                                                                      baseMediaType));
        }
    }

    protected static boolean isReadable(final Class<?>      type,
                                        final MediaType     actualMediaType,
                                        final MediaType ... requiredMediaTypes)
    {
        if (type.getAnnotation(OslcResourceShape.class) != null)
        {
            for (final MediaType requiredMediaType : requiredMediaTypes)
            {
                if (requiredMediaType.isCompatible(actualMediaType))
                {
                    return true;
                }
            }
        }

        return false;
    }

    protected static Object[] readFrom(final Class<?>    type,
                                       final MediaType   errorMediaType,
                                       final InputStream inputStream)
              throws WebApplicationException
    {
        final Model model = ModelFactory.createDefaultModel();

        final RDFReader reader = model.getReader(); // Default reader handles both xml and abbreviated xml
        reader.setErrorHandler(ERROR_HANDLER);

        try
        {
			// Pass the empty string as the base URI. This allows Jena to
			// resolve relative URIs commonly used to in reified statements
			// for OSLC link labels. See this section of the CM specification
			// for an example:
        	// http://open-services.net/bin/view/Main/CmSpecificationV2?sortcol=table;up=#Labels_for_Relationships
        	reader.read(model,
        				inputStream,
        				"");

            return JenaModelHelper.fromJenaModel(model,
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

	private static boolean isOslcQuery(final String parmString)
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