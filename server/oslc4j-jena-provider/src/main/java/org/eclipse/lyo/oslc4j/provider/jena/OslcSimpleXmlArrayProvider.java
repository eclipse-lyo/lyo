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

import javax.ws.rs.Consumes;
import javax.ws.rs.Produces;

import javax.ws.rs.ext.Provider;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;

@Provider
@Produces({OslcMediaType.APPLICATION_XML, OslcMediaType.TEXT_XML})
@Consumes({OslcMediaType.APPLICATION_XML, OslcMediaType.TEXT_XML})
public class OslcSimpleXmlArrayProvider extends OslcSimpleRdfXmlArrayProvider {

}
