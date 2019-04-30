/*******************************************************************************
 * Copyright (c) 2018 Andrew Berezovskyi.
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
 *	   Andrew Berezovskyi - initial implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.provider.jena;

import javax.ws.rs.Consumes;
import javax.ws.rs.Produces;
import javax.ws.rs.ext.Provider;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;

/**
 * Created on 2018-03-03
 *
 * @author Andrew Berezovskyi (andriib@kth.se)
 * @version $version-stub$
 * @since 2.4.0
 */
@Provider
@Produces({OslcMediaType.APPLICATION_JSON_LD})
@Consumes({OslcMediaType.APPLICATION_JSON_LD})
public class OslcJsonLdCollectionProvider extends OslcRdfXmlCollectionProvider { }
