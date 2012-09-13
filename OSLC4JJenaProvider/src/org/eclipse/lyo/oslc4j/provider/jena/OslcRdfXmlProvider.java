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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.annotation.Annotation;
import java.lang.reflect.Type;

import javax.ws.rs.Consumes;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.ext.MessageBodyReader;
import javax.ws.rs.ext.MessageBodyWriter;
import javax.ws.rs.ext.Provider;

import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;

@Provider
@Produces({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_XML, OslcMediaType.TEXT_XML})
@Consumes({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_XML, OslcMediaType.TEXT_XML})
public final class OslcRdfXmlProvider
       extends AbstractOslcRdfXmlProvider
       implements MessageBodyReader<Object>,
                  MessageBodyWriter<Object>
{
    public OslcRdfXmlProvider()
    {
        super();
    }

    @Override
    public long getSize(final Object       object,
                        final Class<?>     type,
                        final Type         genericType,
                        final Annotation[] annotation,
                        final MediaType    mediaType)
    {
        return -1;
    }

    @Override
    public boolean isWriteable(final Class<?>     type,
                               final Type         genericType,
                               final Annotation[] annotations,
                               final MediaType    mediaType)
    {
        return isWriteable(type,
                           annotations,
                           mediaType,
                           OslcMediaType.APPLICATION_RDF_XML_TYPE,
                           OslcMediaType.APPLICATION_XML_TYPE,
                           OslcMediaType.TEXT_XML_TYPE);
    }

    @Override
    public void writeTo(final Object                         object,
                        final Class<?>                       type,
                        final Type                           genericType,
                        final Annotation[]                   annotations,
                        final MediaType                      mediaType,
                        final MultivaluedMap<String, Object> map,
                        final OutputStream                   outputStream)
           throws IOException,
                  WebApplicationException
    {
        writeTo(false,
                new Object[] {object},
                mediaType,
                map,
                outputStream);
    }

    @Override
    public boolean isReadable(final Class<?>     type,
                              final Type         genericType,
                              final Annotation[] annotations,
                              final MediaType    mediaType)
    {
        return isReadable(type,
                          mediaType,
                          OslcMediaType.APPLICATION_RDF_XML_TYPE,
                          OslcMediaType.APPLICATION_XML_TYPE,
                          OslcMediaType.TEXT_XML_TYPE);
    }

    @Override
    public Object readFrom(final Class<Object>                  type,
                           final Type                           genericType,
                           final Annotation[]                   annotations,
                           final MediaType                      mediaType,
                           final MultivaluedMap<String, String> map,
                           final InputStream                    inputStream)
           throws IOException,
                  WebApplicationException
    {
        final Object[] objects = readFrom(type,
                                          mediaType,
                                          map,
                                          inputStream);

        if ((objects != null) &&
            (objects.length > 0))
        {
            return objects[0];
        }

        return null;
    }
}