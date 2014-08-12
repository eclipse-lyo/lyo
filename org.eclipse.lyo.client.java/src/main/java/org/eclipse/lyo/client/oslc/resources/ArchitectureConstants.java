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
 *     Michael Fiedler        - initial implementation
 *******************************************************************************/
package org.eclipse.lyo.client.oslc.resources;


public interface ArchitectureConstants
{
	public static String ARCHITECTURE_DOMAIN        = "http://open-services.net/ns/am#";
	public static String ARCHITECTURE_NAMESPACE     = "http://open-services.net/ns/am#";
	public static String ARCHITECTURE_PREFIX        = "oslc_am";
    public static String FOAF_NAMESPACE                              = "http://xmlns.com/foaf/0.1/";
    public static String FOAF_NAMESPACE_PREFIX                       = "foaf";

    public static String TYPE_ARCHITECTURE_RESOURCE  = ARCHITECTURE_NAMESPACE + "Resource";
    public static String TYPE_ARCHITECTURE_LINK_TYPE = ARCHITECTURE_NAMESPACE + "LinkType";
    public static String TYPE_PERSON                = FOAF_NAMESPACE + "Person";

}
