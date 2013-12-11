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
 *     Russell Boykin       - initial API and implementation
 *     Alberto Giammaria    - initial API and implementation
 *     Chris Peters         - initial API and implementation
 *     Gianluca Bernardini  - initial API and implementation
 *     Steve Pitschke       - Add support for FilteredResource and
 *                            ResponseInfo
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.provider.json4j;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.annotation.Annotation;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.net.URI;
import java.util.Collection;
import java.util.Map;

import javax.ws.rs.Consumes;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.ext.MessageBodyReader;
import javax.ws.rs.ext.MessageBodyWriter;
import javax.ws.rs.ext.Provider;

import org.eclipse.lyo.oslc4j.core.OSLC4JUtils;
import org.eclipse.lyo.oslc4j.core.model.FilteredResource;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;
import org.eclipse.lyo.oslc4j.core.model.ResponseInfo;
import org.eclipse.lyo.oslc4j.core.model.ResponseInfoArray;
import org.eclipse.lyo.oslc4j.core.model.ResponseInfoCollection;

@Provider
@Produces(OslcMediaType.APPLICATION_JSON)
@Consumes(OslcMediaType.APPLICATION_JSON)
public final class OslcRdfJsonProvider
       extends AbstractOslcRdfJsonProvider
       implements MessageBodyReader<Object>,
                  MessageBodyWriter<Object>
{
    public OslcRdfJsonProvider()
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
        Class<?> actualType;
        
        if (FilteredResource.class.isAssignableFrom(type) &&
            (genericType instanceof ParameterizedType))
        {
            ParameterizedType parameterizedType = (ParameterizedType)genericType;
            Type[] actualTypeArguments = parameterizedType.getActualTypeArguments();
            
            if (actualTypeArguments.length != 1)
            {
                return false;
            }
            
            if (actualTypeArguments[0] instanceof Class<?>)
            {
                actualType = (Class<?>)actualTypeArguments[0];
            }
            else if (actualTypeArguments[0] instanceof ParameterizedType)
            {
                parameterizedType = (ParameterizedType)actualTypeArguments[0];
                actualTypeArguments = parameterizedType.getActualTypeArguments();
                
                if (actualTypeArguments.length != 1 ||
                    ! (actualTypeArguments[0] instanceof Class<?>))
                {
                    return false;
                }
                
                actualType = (Class<?>)actualTypeArguments[0];
            }
            else if (actualTypeArguments[0] instanceof GenericArrayType)
            {
                GenericArrayType genericArrayType =
                    (GenericArrayType)actualTypeArguments[0];
                Type componentType = genericArrayType.getGenericComponentType();
                
                if (! (componentType instanceof Class<?>))
                {
                    return false;
                }
                
                actualType = (Class<?>)componentType;
            }
            else
            {
                return false;
            }

            Type rawType = parameterizedType.getRawType();
            if (URI.class.equals(actualType)
                    && (ResponseInfoCollection.class.equals(rawType) || ResponseInfoArray.class.equals(rawType)))
            {
                return true;
            }            
        }
        else
        {
            actualType = type;
        }
        
        return isWriteable(actualType,
                           annotations,
                           OslcMediaType.APPLICATION_JSON_TYPE,
                           mediaType);
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
        Object[]                        objects;
        Map<String, Object>             properties = null;
        String                          descriptionURI = null;
        String                          responseInfoURI = null;
        ResponseInfo<?>					responseInfo = null;
        
        if (object instanceof FilteredResource<?>)
        {
            final FilteredResource<?> filteredResource =
                (FilteredResource<?>)object;
            
            properties = filteredResource.properties();
            
            if (filteredResource instanceof ResponseInfo<?>)
            {                
                responseInfo = (ResponseInfo<?>)filteredResource;
                
                String requestURI = OSLC4JUtils.resolveURI(httpServletRequest, true);
                responseInfoURI = requestURI;
                
                FilteredResource<?> container = responseInfo.getContainer();
                if (container != null)
                {
                    URI containerAboutURI = container.getAbout();
                    if (containerAboutURI != null) 
                    {
                        descriptionURI = containerAboutURI.toString();
                    }
                }                
                if (null == descriptionURI)
                {
                    descriptionURI = requestURI;
                }
                
                final String queryString = httpServletRequest.getQueryString();
                
                if ((queryString != null) &&
                    (isOslcQuery(queryString)))
                {
                    responseInfoURI += "?" + queryString;
                }
                
                if (filteredResource instanceof ResponseInfoArray<?>)
                {
                    objects = ((ResponseInfoArray<?>)filteredResource).array();
                }
                else
                {
                    Collection<?> collection =
                        ((ResponseInfoCollection<?>)filteredResource).collection();
                    
                    objects = collection.toArray(new Object[collection.size()]);
                }
            }
            else
            {
                Object nestedObject = filteredResource.resource();
                
                if (nestedObject instanceof Object[])
                {
                    objects = (Object[])nestedObject;
                }
                else if (nestedObject instanceof Collection<?>)
                {
                    objects = ((Collection<?>)nestedObject).toArray();
                }
                else
                {
                    objects = new Object[] { object };
                }
            }
        }
        else
        {
            objects = new Object[] { object };
        }
        
        writeTo(objects,
                mediaType,
                map,
                outputStream,
                properties,
                descriptionURI,
                responseInfoURI,
                responseInfo);
    }

    @Override
    public boolean isReadable(final Class<?>     type,
                              final Type         genericType,
                              final Annotation[] annotations,
                              final MediaType    mediaType)
    {
        return isReadable(type,
                          OslcMediaType.APPLICATION_JSON_TYPE,
                          mediaType);
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