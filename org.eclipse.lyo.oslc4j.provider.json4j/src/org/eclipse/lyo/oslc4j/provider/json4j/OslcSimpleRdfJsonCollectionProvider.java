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
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.provider.json4j;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.annotation.Annotation;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
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

import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;

@Provider
@Produces(OslcMediaType.APPLICATION_JSON)
@Consumes(OslcMediaType.APPLICATION_JSON)
public final class OslcSimpleRdfJsonCollectionProvider
       extends OslcRdfJsonCollectionProvider
{
    public OslcSimpleRdfJsonCollectionProvider()
    {
        super();
    }

    @Override
    public void writeTo(final Collection<Object>             collection,
                        final Class<?>                       type,
                        final Type                           genericType,
                        final Annotation[]                   annotations,
                        final MediaType                      mediaType,
                        final MultivaluedMap<String, Object> map,
                        final OutputStream                   outputStream)
           throws IOException,
                  WebApplicationException
    {
        writeTo(collection.toArray(new Object[collection.size()]),
                mediaType,
                map,
                outputStream,
                null,
                null,
                null,
                null);
    }
}