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
package org.eclipse.lyo.oslc4j.provider.jena.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.WebApplicationException;
import jakarta.ws.rs.core.MediaType;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Proxy;
import java.lang.reflect.Type;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.eclipse.lyo.oslc4j.core.model.QueryCapability;
import org.eclipse.lyo.oslc4j.core.model.ResponseInfo;
import org.eclipse.lyo.oslc4j.core.model.ResponseInfoCollection;
import org.eclipse.lyo.oslc4j.core.model.Service;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.eclipse.lyo.oslc4j.provider.jena.AbstractOslcRdfXmlProvider;
import org.eclipse.lyo.oslc4j.provider.jena.OslcRdfXmlCollectionProvider;
import org.eclipse.lyo.oslc4j.provider.jena.OslcRdfXmlProvider;
import org.eclipse.lyo.oslc4j.provider.jena.RdfXmlAbbreviatedWriter;
import org.junit.Test;

public class RdfXmlAbbreviatedWriterTest {
    @Test
    public void testCircularReference() {
        InputStream is = RdfXmlAbbreviatedWriterTest.class.getResourceAsStream("/circular.xml");
        assertNotNull("Could not read file: circular.xml", is);
        Model m = ModelFactory.createDefaultModel();
        m.read(is, null);
        RdfXmlAbbreviatedWriter w = new RdfXmlAbbreviatedWriter();
        w.write(m, System.out, null);
    }

    /**
     * Scenario tested with cyclic reference http://server/oslc/pr/collection:
     * {@code
     * <rdf:RDF>
     *	   <rdfs:Container rdf:about = "http://server/oslc/pr/collection">
     *		   <rdfs:member>
     *			   <oslc:ServiceProvider rdf:about = "http://test:9999/inlineResources/self-circular.xml">
     *				   <oslc:service>
     *					   <oslc:Service>
     *						   <oslc:queryCapability>
     *							   <oslc:QueryCapability>
     *								   <oslc:queryBase rdf:resource = "http://server/oslc/pr/collection"/>
     *							   </oslc:QueryCapability>
     *						   </oslc:queryCapability>
     *					   </oslc:Service>
     *				   </oslc:service>
     *			   </oslc:ServiceProvider>
     *		   </rdfs:member>
     *	   </rdfs:Container>
     * </rdf:RDF>
     * }
     */
    @Test
    public void testSelfCircularReference()
            throws WebApplicationException,
                    IOException,
                    SecurityException,
                    NoSuchFieldException,
                    IllegalArgumentException,
                    IllegalAccessException {

        // Cannot read a payload with this scenario because the reader will define
        // the collection as an inline resource, preventing the bug we want to test.

        ServiceProvider sp = new ServiceProvider();
        sp.setAbout(URI.create("http://test:9999/inlineResources/self-circular.xml"));
        Service service = new Service();
        sp.addService(service);
        QueryCapability queryCapability = new QueryCapability();
        service.addQueryCapability(queryCapability);
        queryCapability.setQueryBase(URI.create("http://server/pr/collection"));

        List<ServiceProvider> list = new ArrayList<>(1);
        list.add(sp);

        ResponseInfoCollection<ServiceProvider> info =
                new ResponseInfoCollection<>(list, null, 1, (String) null);
        info.setAbout(URI.create("http://server/pr/collection?oslc.select=*"));
        info.getContainer().setAbout(URI.create("http://server/pr/collection"));

        // Since we do not have a Mock API, creating a proxy to mock the
        // request.
        HttpServletRequest proxyInstance =
                (HttpServletRequest)
                        Proxy.newProxyInstance(
                                HttpServletRequest.class.getClassLoader(),
                                new Class<?>[] {HttpServletRequest.class},
                                (proxy, method, args) -> {
                                    if (method.getReturnType().isPrimitive()) {
                                        return 0;
                                    }
                                    return null;
                                });

        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        OslcRdfXmlProvider op = new OslcRdfXmlProvider();
        Field requestURIField =
                AbstractOslcRdfXmlProvider.class.getDeclaredField("httpServletRequest");
        requestURIField.setAccessible(true);
        requestURIField.set(op, proxyInstance);
        op.writeTo(
                info,
                ResponseInfo.class,
                ServiceProvider.class,
                null,
                MediaType.APPLICATION_XML_TYPE,
                null,
                baos);

        ParameterizedType paramType =
                new ParameterizedType() {

                    @Override
                    public Type getRawType() {
                        return List.class;
                    }

                    @Override
                    public Type getOwnerType() {
                        return null;
                    }

                    @Override
                    public Type[] getActualTypeArguments() {

                        return new Type[] {ServiceProvider.class};
                    }
                };

        System.out.println(baos.toString());

        OslcRdfXmlCollectionProvider ocp = new OslcRdfXmlCollectionProvider();

        @SuppressWarnings({"unchecked"})
        Collection<Object> col =
                ocp.readFrom(
                        (Class) List.class,
                        paramType,
                        null,
                        MediaType.APPLICATION_XML_TYPE,
                        null,
                        new ByteArrayInputStream(baos.toByteArray()));

        assertEquals("Unable to read a collection with cyclic reference", 1, col.size());
    }
}
