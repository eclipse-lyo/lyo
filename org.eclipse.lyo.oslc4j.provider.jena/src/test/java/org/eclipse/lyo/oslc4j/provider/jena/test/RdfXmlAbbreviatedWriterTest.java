/*******************************************************************************
 * Copyright (c) 2013 IBM Corporation.
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
 *     Samuel Padgett - initial implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.provider.jena.test;

import static org.junit.Assert.assertNotNull;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Proxy;
import java.lang.reflect.Type;
import java.net.URI;
import java.security.Principal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.AsyncContext;
import javax.servlet.DispatcherType;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletInputStream;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.servlet.http.Part;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;

import junit.framework.Assert;

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

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;

public class RdfXmlAbbreviatedWriterTest {
	@Test
	public void testCircularReference()
	{
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
     *     <rdfs:Container rdf:about = "http://server/oslc/pr/collection">
     *         <rdfs:member>
     *             <oslc:ServiceProvider rdf:about = "http://test:9999/inlineResources/self-circular.xml">
     *                 <oslc:service>
     *                     <oslc:Service>
     *                         <oslc:queryCapability>
     *                             <oslc:QueryCapability>
     *                                 <oslc:queryBase rdf:resource = "http://server/oslc/pr/collection"/>
     *                             </oslc:QueryCapability>
     *                         </oslc:queryCapability>
     *                     </oslc:Service>
     *                 </oslc:service>
     *             </oslc:ServiceProvider>
     *         </rdfs:member>
     *     </rdfs:Container>
     * </rdf:RDF>
	 * }
	 */
    @Test
    public void testSelfCircularReference() throws WebApplicationException, IOException, SecurityException,
            NoSuchFieldException, IllegalArgumentException, IllegalAccessException {
        
        // Cannot read a payload with this scenario because the reader will define
        // the collection as an inline resource, preventing the bug we want to test.
        
        ServiceProvider sp = new ServiceProvider();
        sp.setAbout(URI.create("http://test:9999/inlineResources/self-circular.xml"));
        Service service = new Service();
        sp.addService(service);
        QueryCapability queryCapability = new QueryCapability();
        service.addQueryCapability(queryCapability);
        queryCapability.setQueryBase(URI.create("http://server/pr/collection"));

        List<ServiceProvider> list = new ArrayList<ServiceProvider>(1);
        list.add(sp);

        ResponseInfoCollection<ServiceProvider> info = new ResponseInfoCollection<ServiceProvider>(list, null, 1,
                (String) null);
        info.setAbout(URI.create("http://server/pr/collection?oslc.select=*"));
        info.getContainer().setAbout(URI.create("http://server/pr/collection"));
        
        // Since we do not have a Mock API, creating a proxy to mock the
        // request.
        HttpServletRequest proxy = (HttpServletRequest) Proxy.newProxyInstance(
                HttpServletRequest.class.getClassLoader(), new Class<?>[] { HttpServletRequest.class },
                new InvocationHandler() {
                    @Override
                    public Object invoke(Object arg0, Method arg1, Object[] arg2) throws Throwable {
                        if (arg1.getReturnType().isPrimitive()) {
                            return 0;
                        }
                        return null;
                    }
                });
        
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        OslcRdfXmlProvider op = new OslcRdfXmlProvider();
        Field requestURIField = AbstractOslcRdfXmlProvider.class.getDeclaredField("httpServletRequest");
        requestURIField.setAccessible(true);
        requestURIField.set(op, proxy);
        op.writeTo(info, ResponseInfo.class, ServiceProvider.class, null, MediaType.APPLICATION_XML_TYPE, null, baos);

        ParameterizedType paramType = new ParameterizedType() {

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

                return new Type[] { ServiceProvider.class };
            }
        };

        System.out.println(baos.toString());

        OslcRdfXmlCollectionProvider ocp = new OslcRdfXmlCollectionProvider();
        Collection col = ocp.readFrom((Class) List.class, paramType, null, MediaType.APPLICATION_XML_TYPE, null,
                new ByteArrayInputStream(baos.toByteArray()));

        Assert.assertEquals("Unable to read a collection with cyclic reference", 1, col.size());

    }
	
}
