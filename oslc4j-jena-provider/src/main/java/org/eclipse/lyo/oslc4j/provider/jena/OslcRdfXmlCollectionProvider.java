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
package org.eclipse.lyo.oslc4j.provider.jena;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.annotation.Annotation;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.net.URI;
import java.util.AbstractCollection;
import java.util.AbstractList;
import java.util.AbstractSequentialList;
import java.util.AbstractSet;
import java.util.Arrays;
import java.util.Collection;
import java.util.Deque;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.NavigableSet;
import java.util.Queue;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.ws.rs.Consumes;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.ext.MessageBodyReader;
import javax.ws.rs.ext.MessageBodyWriter;
import javax.ws.rs.ext.Provider;

import org.eclipse.lyo.oslc4j.core.annotation.OslcNotQueryResult;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;

@Provider
@Produces({OslcMediaType.APPLICATION_RDF_XML})
@Consumes({OslcMediaType.APPLICATION_RDF_XML})
public class OslcRdfXmlCollectionProvider
	   extends AbstractOslcRdfXmlProvider
	   implements MessageBodyReader<Collection<Object>>,
				  MessageBodyWriter<Collection<Object>>
{
	public OslcRdfXmlCollectionProvider()
	{
		super();
	}

	@Override
	public long getSize(final Collection<Object> collection,
						final Class<?>			 type,
						final Type				 genericType,
						final Annotation[]		 annotation,
						final MediaType			 mediaType)
	{
		return -1;
	}

	@Override
	public boolean isWriteable(final Class<?>	  type,
							   final Type		  genericType,
							   final Annotation[] annotations,
							   final MediaType	  mediaType)
	{
		if ((Collection.class.isAssignableFrom(type)) &&
			(genericType instanceof ParameterizedType))
		{
			final ParameterizedType parameterizedType = (ParameterizedType) genericType;

			final Type[] actualTypeArguments = parameterizedType.getActualTypeArguments();

			if (actualTypeArguments.length == 1)
			{
				final Type actualTypeArgument = actualTypeArguments[0];

				if (actualTypeArgument instanceof Class)
				{
					return isWriteable((Class<?>) actualTypeArgument,
									   annotations,
									   mediaType,
									   OslcMediaType.APPLICATION_RDF_XML_TYPE,
									   OslcMediaType.APPLICATION_XML_TYPE,
									   OslcMediaType.TEXT_XML_TYPE, 
									   OslcMediaType.TEXT_TURTLE_TYPE,
									   OslcMediaType.APPLICATION_JSON_LD_TYPE);
				}
			}
		}

		return false;
	}

	@Override
	public void writeTo(final Collection<Object>			 collection,
						final Class<?>						 type,
						final Type							 genericType,
						final Annotation[]					 annotations,
						final MediaType						 mediaType,
						final MultivaluedMap<String, Object> map,
						final OutputStream outputStream)
		   throws IOException,
				  WebApplicationException
	{
		final ParameterizedType parameterizedType = (ParameterizedType) genericType;
		final Type[] actualTypeArguments = parameterizedType.getActualTypeArguments();

		writeTo(JenaProviderHelper.isQueryResult((Class<?>)actualTypeArguments[0], annotations),
				collection.toArray(new Object[0]),
				mediaType,
				map,
				outputStream);
	}

	@Override
	public boolean isReadable(final Class<?>	 type,
							  final Type		 genericType,
							  final Annotation[] annotations,
							  final MediaType	 mediaType)
	{
		if ((Collection.class.isAssignableFrom(type)) &&
			(genericType instanceof ParameterizedType))
		{
			final ParameterizedType parameterizedType = (ParameterizedType) genericType;

			final Type[] actualTypeArguments = parameterizedType.getActualTypeArguments();

			if (actualTypeArguments.length == 1)
			{
				final Type actualTypeArgument = actualTypeArguments[0];

				if (URI.class.equals((Class<?>) actualTypeArgument)) 
				{
					if (isCompatible(mediaType,
							OslcMediaType.APPLICATION_RDF_XML_TYPE,
							OslcMediaType.APPLICATION_XML_TYPE,
							OslcMediaType.TEXT_XML_TYPE,
							OslcMediaType.TEXT_TURTLE_TYPE))
					{
						return true;
					}					 
				} 
				else if (actualTypeArgument instanceof Class)
				{
					return isReadable((Class<?>) actualTypeArgument,
									  mediaType,
									  OslcMediaType.APPLICATION_RDF_XML_TYPE,
									  OslcMediaType.APPLICATION_XML_TYPE,
									  OslcMediaType.TEXT_XML_TYPE,
									  OslcMediaType.TEXT_TURTLE_TYPE);
				}
			}
		}

		return false;
	}

	@Override
	public Collection<Object> readFrom(final Class<Collection<Object>>		type,
									   final Type							genericType,
									   final Annotation[]					annotations,
									   final MediaType						mediaType,
									   final MultivaluedMap<String, String> map,
									   final InputStream					inputStream)
		   throws IOException,
				  WebApplicationException
	{
		if (genericType instanceof ParameterizedType)
		{
			final ParameterizedType parameterizedType = (ParameterizedType) genericType;

			final Type[] actualTypeArguments = parameterizedType.getActualTypeArguments();

			if (actualTypeArguments.length == 1)
			{
				final Type actualTypeArgument = actualTypeArguments[0];

				if (actualTypeArgument instanceof Class)
				{
					final Object[] objects = readFrom((Class<?>) actualTypeArgument,
													  mediaType,
													  map,
													  inputStream);

					final Collection<Object> collection;

					// Handle the Collection, List, Deque, Queue interfaces.
					// Handle the AbstractCollection, AbstractList, AbstractSequentialList classes
					if ((Collection.class.equals(type)) ||
						(List.class.equals(type))||
						(Deque.class.equals(type)) ||
						(Queue.class.equals(type)) ||
						(AbstractCollection.class.equals(type)) ||
						(AbstractList.class.equals(type)) ||
						(AbstractSequentialList.class.equals(type)))
					{
						collection = new LinkedList<Object>();
					}
					// Handle the Set interface
					// Handle the AbstractSet class
					else if ((Set.class.equals(type)) ||
							 (AbstractSet.class.equals(type)))
					{
						collection = new HashSet<Object>();
					}
					// Handle the SortedSet and NavigableSet interfaces
					else if ((SortedSet.class.equals(type)) ||
							 (NavigableSet.class.equals(type)))
					{
						collection = new TreeSet<Object>();
					}
					// Not handled above.  Let's try newInstance with possible failure.
					else
					{
						try
						{
							@SuppressWarnings("cast")
							final Collection<Object> tempCollection = ((Collection<Object>) type.newInstance());

							collection = tempCollection;
						}
						catch (final Exception exception)
						{
							throw new WebApplicationException(exception,
															  buildBadRequestResponse(exception,
																					  mediaType,
																					  map));
						}
					}

					collection.addAll(Arrays.asList(objects));

					return collection;
				}
			}
		}

		return null;
	}
}
