/*
 * Copyright (c) 2023 Contributors to the Eclipse Foundation
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

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertEquals;

import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

import javax.xml.datatype.DatatypeConfigurationException;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ReifiedStatement;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.eclipse.lyo.oslc4j.provider.jena.test.resources.ResourceWithReifiedLinks;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ReifiedLinksTest {

    private final static Logger log = LoggerFactory.getLogger(ReifiedLinksTest.class);

    static String uriOfLinkWithNoLabel = "http://example.com/link.with.no.label";
    static String uriOfLinkWithLabel = "http://example.com/link.with.label";

    @Test
    public void testPropertiesPreserved() {
        ResourceWithReifiedLinks resourceOriginal = buildResource();

        ResourceWithReifiedLinks resourceExtracted = extract(model(resourceOriginal));

        assertEquals(resourceOriginal.getTitle(), resourceExtracted.getTitle());
        assertEquals(resourceOriginal.getLinkWithNoLabel().getValue(),
                resourceExtracted.getLinkWithNoLabel().getValue());
        assertEquals(resourceOriginal.getLinkWithLabel().getValue(),
                resourceExtracted.getLinkWithLabel().getValue());
    }

    @Test
    public void testLabelIsPreserved() {
        ResourceWithReifiedLinks resourceOriginal = buildResource();

        ResourceWithReifiedLinks resourceExtracted = extract(model(resourceOriginal));

        assertEquals(resourceOriginal.getLinkWithLabel().getLabel(),
                resourceExtracted.getLinkWithLabel().getLabel());
    }

    @Test
    public void testLinkWoLabelIsNotReified() {
        ResourceWithReifiedLinks resourceOriginal = buildResource();

        final Model model = model(resourceOriginal);

        final List<ReifiedStatement> reifiedStatements = model.listReifiedStatements().toList();
        assertEquals("Only 'linkWithLabel' property should be reified", 1,
                reifiedStatements.size());
    }

    @Test
    public void testReifiedStmtIsNotEmpty() {
        ResourceWithReifiedLinks resourceOriginal = buildResource();

        final Model model = model(resourceOriginal);

        final List<ReifiedStatement> reifiedStatements = model.listReifiedStatements().toList();
        assertThat(reifiedStatements).isNotEmpty();
        for (ReifiedStatement statement : reifiedStatements) {
            // A reified statement in Lyo is serialised with 4 triples for subject, predicate,
            // object, type. If it has no other info, we consider it to be empty.
            assertThat(statement.listProperties().toList().size()).isGreaterThan(4);
        }

    }

    private Model model(ResourceWithReifiedLinks resource) {
        try {
            return JenaModelHelper.createJenaModel(new Object[]{resource});
        } catch (DatatypeConfigurationException | IllegalAccessException |
                InvocationTargetException | OslcCoreApplicationException e) {
            throw new IllegalArgumentException("Jena model can't be created from the resource", e);
        }
    }

    private ResourceWithReifiedLinks extract(Model m) {
        try {
            final Object[] objects = JenaModelHelper.fromJenaModel(m,
                    ResourceWithReifiedLinks.class);
            if (objects.length != 1) {
                log.error("{} objects in a model", objects.length);
                throw new IllegalStateException(
                        "Jena model must have exactly one ResourceWithReifiedLinks object");
            }
            return (ResourceWithReifiedLinks) objects[0];
        } catch (DatatypeConfigurationException | IllegalAccessException | InstantiationException
                | OslcCoreApplicationException | InvocationTargetException | URISyntaxException |
                NoSuchMethodException e) {
            throw new IllegalArgumentException("Cannot deserialise the resource from model", e);
        }
    }

    private static ResourceWithReifiedLinks buildResource() {
        ResourceWithReifiedLinks aResource = null;
        try {
            final URI about = new URI("http://example.com/someResource");
            aResource = new ResourceWithReifiedLinks(about);
            aResource.setTitle("Some Title");
            aResource.setLinkWithNoLabel(new Link(new URI(uriOfLinkWithNoLabel)));
            Link l = new Link(new URI(uriOfLinkWithLabel));
            l.setLabel("some label for a link to reify");
            aResource.setLinkWithLabel(l);
        } catch (URISyntaxException e) {
            log.error("URI is malformed", e);
        }

        return aResource;
    }
}
