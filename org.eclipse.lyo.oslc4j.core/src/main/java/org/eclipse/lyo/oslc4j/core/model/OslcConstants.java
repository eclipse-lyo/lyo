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
 *	   Russell Boykin		- initial API and implementation
 *	   Alberto Giammaria	- initial API and implementation
 *	   Chris Peters			- initial API and implementation
 *	   Gianluca Bernardini	- initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.core.model;

public interface OslcConstants {
	public static final String OSLC_CORE_DOMAIN = "http://open-services.net/ns/core#";

	public static final String DCTERMS_NAMESPACE		= "http://purl.org/dc/terms/";
	public static final String OSLC_CORE_NAMESPACE		= "http://open-services.net/ns/core#";
	public static final String OSLC_DATA_NAMESPACE		= "http://open-services.net/ns/servicemanagement/1.0/";
	public static final String RDF_NAMESPACE			= "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
	public static final String RDFS_NAMESPACE			= "http://www.w3.org/2000/01/rdf-schema#";
	public static final String XML_NAMESPACE			= "http://www.w3.org/2001/XMLSchema#";

	public static final String DCTERMS_NAMESPACE_PREFIX	  = "dcterms";
	public static final String OSLC_CORE_NAMESPACE_PREFIX = "oslc";
	public static final String OSLC_DATA_NAMESPACE_PREFIX = "oslc_data";
	public static final String RDF_NAMESPACE_PREFIX		  = "rdf";
	public static final String RDFS_NAMESPACE_PREFIX	  = "rdfs";

	public static final String OSLC_USAGE_DEFAULT = "http://open-services.net/ns/core#default";

	public static final String TYPE_ALLOWED_VALUES			 = OSLC_CORE_NAMESPACE + "AllowedValues";
	public static final String TYPE_COMPACT					 = OSLC_CORE_NAMESPACE + "Compact";
	public static final String TYPE_CREATION_FACTORY		 = OSLC_CORE_NAMESPACE + "CreationFactory";
	public static final String TYPE_DIALOG					 = OSLC_CORE_NAMESPACE + "Dialog";
	public static final String TYPE_ERROR					 = OSLC_CORE_NAMESPACE + "Error";
	public static final String TYPE_EXTENDED_ERROR			 = OSLC_CORE_NAMESPACE + "ExtendedError";
	public static final String TYPE_O_AUTH_CONFIGURATION	 = OSLC_CORE_NAMESPACE + "OAuthConfiguration";
	public static final String TYPE_PREFIX_DEFINITION		 = OSLC_CORE_NAMESPACE + "PrefixDefinition";
	public static final String TYPE_PREVIEW					 = OSLC_CORE_NAMESPACE + "Preview";
	public static final String TYPE_PROPERTY				 = OSLC_CORE_NAMESPACE + "Property";
	public static final String TYPE_PUBLISHER				 = OSLC_CORE_NAMESPACE + "Publisher";
	public static final String TYPE_QUERY_CAPABILITY		 = OSLC_CORE_NAMESPACE + "QueryCapability";
	public static final String TYPE_RESOURCE_SHAPE			 = OSLC_CORE_NAMESPACE + "ResourceShape";
	public static final String TYPE_RESPONSE_INFO			 = OSLC_CORE_NAMESPACE + "ResponseInfo";
	public static final String TYPE_SERVICE					 = OSLC_CORE_NAMESPACE + "Service";
	public static final String TYPE_SERVICE_PROVIDER		 = OSLC_CORE_NAMESPACE + "ServiceProvider";
	public static final String TYPE_SERVICE_PROVIDER_CATALOG = OSLC_CORE_NAMESPACE + "ServiceProviderCatalog";

	public static final String TYPE_CONTAINER				 = RDFS_NAMESPACE + "Container";
	public static final String PATH_RESOURCE_SHAPES = "resourceShapes";

	public static final String PATH_ALLOWED_VALUES			 = "allowedValues";
	public static final String PATH_CREATION_FACTORY		 = "creationFactory";
	public static final String PATH_COMPACT					 = "compact";
	public static final String PATH_DIALOG					 = "dialog";
	public static final String PATH_ERROR					 = "error";
	public static final String PATH_EXTENDED_ERROR			 = "extendedError";
	public static final String PATH_OAUTH_CONFIGURATION		 = "oauthConfiguration";
	public static final String PATH_PREFIX_DEFINITION		 = "prefixDefinition";
	public static final String PATH_PREVIEW					 = "preview";
	public static final String PATH_PROPERTY				 = "property";
	public static final String PATH_PUBLISHER				 = "publisher";
	public static final String PATH_QUERY_CAPABILITY		 = "queryCapability";
	public static final String PATH_RESOURCE_SHAPE			 = "resourceShape";
	public static final String PATH_SERVICE					 = "service";
	public static final String PATH_SERVICE_PROVIDER		 = "serviceProvider";
	public static final String PATH_SERVICE_PROVIDER_CATALOG = "serviceProviderCatalog";
}
