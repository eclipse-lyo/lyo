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

package org.eclipse.lyo.oslc4j.provider.json4j.test;

import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

import org.eclipse.lyo.oslc4j.provider.json4j.OslcRdfJsonProvider;
import org.eclipse.lyo.oslc4j.provider.json4j.test.resources.TestResource;
import org.junit.jupiter.api.Test;

import jakarta.ws.rs.WebApplicationException;

@SuppressWarnings("deprecation")
public class TestInvalidTypesJson {
    @Test
    public void testInvalidJavaAboutRelativeURI() throws IOException, URISyntaxException {
        assertThrows(WebApplicationException.class, () -> {

            final TestResource relativeUri = new TestResource();

            relativeUri.setAbout(new URI("relative"));

            final OslcRdfJsonProvider oslcRdfJsonProvider = new OslcRdfJsonProvider();

            oslcRdfJsonProvider.writeTo(relativeUri, null, null, null, null, null, null);
        });

    }
}
