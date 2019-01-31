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
 *     Gabriel Ruelas       - initial API and implementation
 *     Carlos A Arreola     - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.client.oslc.resources;

import javax.xml.namespace.QName;

import org.eclipse.lyo.oslc4j.core.model.OslcConstants;

import org.apache.jena.datatypes.BaseDatatype;

public interface RmConstants
{
    public static String REQUIREMENTS_MANAGEMENT_DOMAIN                    = "http://open-services.net/ns/rm#";
    public static String REQUIREMENTS_MANAGEMENT_NAMESPACE                 = "http://open-services.net/ns/rm#";


    public static String REQUIREMENTS_MANAGEMENT_PREFIX              = "oslc_rm";
    public static String SOFTWARE_CONFIGURATION_MANAGEMENT_PREFIX    = "oslc_scm";
    public static String QUALITY_MANAGEMENT_PREFIX                   = "oslc_qm";


    public static String FOAF_NAMESPACE                              = "http://xmlns.com/foaf/0.1/";
    public static String FOAF_NAMESPACE_PREFIX                       = "foaf";

    public static String TYPE_DISCUSSION            = OslcConstants.OSLC_CORE_NAMESPACE + "Discussion";
    public static String TYPE_PERSON                = FOAF_NAMESPACE + "Person";
    public static String TYPE_REQUIREMENT           = REQUIREMENTS_MANAGEMENT_NAMESPACE + "Requirement";
    public static String TYPE_REQUIREMENT_COLLECTION           = REQUIREMENTS_MANAGEMENT_NAMESPACE + "RequirementCollection";


    public static String JAZZ_RM_NAMESPACE                 = "http://jazz.net/ns/rm#";
    public static String JAZZ_RM_NAV_NAMESPACE             = "http://jazz.net/ns/rm/navigation#";
    public static String JAZZ_RM_ACCESS_NAMESPACE         = "http://jazz.net/ns/acp#";

    QName PROPERTY_PRIMARY_TEXT   = new QName(RmConstants.JAZZ_RM_NAMESPACE, "primaryText");
    QName PROPERTY_PARENT_FOLDER  = new QName(RmConstants.JAZZ_RM_NAV_NAMESPACE, "parent");
    QName PROPERTY_ACCESS_CONTROL = new QName(RmConstants.JAZZ_RM_ACCESS_NAMESPACE, "accessControl");



    public static final BaseDatatype RDF_TYPE_BOOLEAN 	= new BaseDatatype(OslcConstants.RDF_NAMESPACE + "boolean");
    public static final BaseDatatype RDF_TYPE_DATETIME 	= new BaseDatatype(OslcConstants.RDF_NAMESPACE + "dateTime");
    public static final BaseDatatype RDF_TYPE_DECIMAL	 = new BaseDatatype(OslcConstants.RDF_NAMESPACE + "decimal");
    public static final BaseDatatype RDF_TYPE_DOUBLE	 = new BaseDatatype(OslcConstants.RDF_NAMESPACE + "double");
    public static final BaseDatatype RDF_TYPE_FLOAT		 = new BaseDatatype(OslcConstants.RDF_NAMESPACE + "float");
    public static final BaseDatatype RDF_TYPE_INTEGER	 = new BaseDatatype(OslcConstants.RDF_NAMESPACE + "integer");
    public static final BaseDatatype RDF_TYPE_STRING	 = new BaseDatatype(OslcConstants.RDF_NAMESPACE + "string");
    public static final BaseDatatype RDF_TYPE_XMLLITERAL = new BaseDatatype(OslcConstants.RDF_NAMESPACE + "XMLLiteral");
    public static final BaseDatatype RDF_TYPE_RESOURCE	 = new BaseDatatype(OslcConstants.RDF_NAMESPACE + "Resource");
    public static final BaseDatatype RDF_TYPE_LOCALRESOURCE	 = new BaseDatatype(OslcConstants.RDF_NAMESPACE + "LocalResource");

    String NAMESPACE_URI_XHTML       = "http://www.w3.org/1999/xhtml"; //$NON-NLS-1$




}
