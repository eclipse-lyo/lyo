/*
 * Copyright (c) 2020 Contributors to the Eclipse Foundation
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
package org.eclipse.lyo.oslc4j.provider.jena;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.annotation.Annotation;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
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

import org.eclipse.lyo.oslc4j.core.CoreHelper;
import org.eclipse.lyo.oslc4j.core.OSLC4JUtils;
import org.eclipse.lyo.oslc4j.core.model.FilteredResource;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;
import org.eclipse.lyo.oslc4j.core.model.ResponseInfo;
import org.eclipse.lyo.oslc4j.core.model.ResponseInfoArray;
import org.eclipse.lyo.oslc4j.core.model.ResponseInfoCollection;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Russell Boykin, Alberto Giammaria, Chris Peters, Gianluca Bernardini, Steve Pitschke
 */
@Provider
@Produces({OslcMediaType.APPLICATION_RDF_XML})
@Consumes({OslcMediaType.APPLICATION_RDF_XML})
public class OslcRdfXmlProvider
	   extends AbstractOslcRdfXmlProvider
	   implements MessageBodyReader<Object>,
				  MessageBodyWriter<Object>
{
    private final static Logger log = LoggerFactory.getLogger(OslcRdfXmlProvider.class);

    public OslcRdfXmlProvider()
	{
		super();
	}

	@Override
	public boolean isWriteable(final Class<?>	  type,
							   final Type		  genericType,
							   final Annotation[] annotations,
							   final MediaType	  mediaType)
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

                if (actualTypeArguments.length == 1) {

                    Type firstTypeArg = actualTypeArguments[0];
                    if (firstTypeArg instanceof Class) {
                        if (log.isTraceEnabled()) {
                            log.trace("isWritable() call on a generic type arg: <{}> (comptime)",
                                CoreHelper.getActualTypeArgument(firstTypeArg).getSimpleName());
                        }
                        return true;
                    } else if (firstTypeArg instanceof TypeVariable) {
                        if (log.isTraceEnabled()) {
                            log.trace("isWritable() call on a generic type arg: <{}> (runtime)",
                                CoreHelper.getActualTypeArgument(firstTypeArg).getSimpleName());
                        }
                        return true;
                    }
                    return false;
                } else {
                    return false;
                }

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

		return ProviderHelper.isSingleLyoResourceType(actualType);
	}

	@Override
	public void writeTo(final Object						 object,
						final Class<?>						 type,
						final Type							 genericType,
						final Annotation[]					 annotations,
						final MediaType						 mediaType,
						final MultivaluedMap<String, Object> map,
						final OutputStream					 outputStream)
		   throws IOException,
				  WebApplicationException
	{
		Object[]						objects;
		Map<String, Object>				properties = null;
		String							descriptionURI = null;
		String							responseInfoURI = null;
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
					(ProviderHelper.isOslcQuery(queryString)))
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

					objects = collection.toArray(new Object[0]);
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
	public boolean isReadable(final Class<?>	 type,
							  final Type		 genericType,
							  final Annotation[] annotations,
							  final MediaType	 mediaType)
	{
		return ProviderHelper.isSingleResourceType(type);
	}

	@Override
	public Object readFrom(final Class<Object>					type,
						   final Type							genericType,
						   final Annotation[]					annotations,
						   final MediaType						mediaType,
						   final MultivaluedMap<String, String> map,
						   final InputStream					inputStream)
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
			// Fix for https://bugs.eclipse.org/bugs/show_bug.cgi?id=412755
			if (OSLC4JUtils.useBeanClassForParsing() && objects.length > 1) {
				throw new IOException("Object length should not be greater than 1.");
			}

			return objects[0];
		}

		return null;
	}

	@Override
	public long getSize(final Object object, final Class<?> type, final Type genericType,
			final Annotation[] annotation, final MediaType mediaType) {
		return ProviderHelper.CANNOT_BE_DETERMINED_IN_ADVANCE;
	}
}
