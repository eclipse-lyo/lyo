/*
 * Copyright (c) 2012-2019 IBM Corporation and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */
package org.eclipse.lyo.oslc4j.provider.jena;

import java.io.IOException;
import java.io.OutputStream;
import java.lang.annotation.Annotation;
import java.lang.reflect.Type;
import java.util.Collection;

import javax.ws.rs.Consumes;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.ext.Provider;

import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;

/**
 * @author Russell Boykin, Alberto Giammaria, Chris Peters, Gianluca Bernardini, Andrew Berezovskyi
 */
@Provider
@Produces({OslcMediaType.APPLICATION_RDF_XML})
@Consumes({OslcMediaType.APPLICATION_RDF_XML})
public class OslcSimpleRdfXmlCollectionProvider
	   extends OslcRdfXmlCollectionProvider
{
	public OslcSimpleRdfXmlCollectionProvider()
	{
		super();
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
		writeTo(collection.toArray(new Object[0]),
				mediaType,
				map,
				outputStream,
				null,
				null,
				null,
				null);
	}
}
