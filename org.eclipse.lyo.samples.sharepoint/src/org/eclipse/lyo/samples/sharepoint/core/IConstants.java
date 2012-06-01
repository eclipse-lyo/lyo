/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
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
 *     Keith Wells             - initial API and implementation
 *     Sam Padgett           - initial API and Implementation
 *     Jim Conallen           - initial API and implementation
 *
 *******************************************************************************/
package org.eclipse.lyo.samples.sharepoint.core;

@SuppressWarnings("nls")
public interface IConstants extends IShareConstants {
	
	// service segments
	public static final String SERVICE_SERVICES = "services";  
	public static final String SERVICE_QUERY = "query";  
	public static final String SERVICE_USER = "user";  
	
	// prefixed names and namespaces
	public static final String XML_DECLARATION = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"; 
	public static final String XML_BASE = "xml:base"; 
	public static final String XMLNS = "xmlns";  
	
	// XSD
	public static final String XSD_DATATYPE_BOOLEAN = "http://www.w3.org/2001/XMLSchema#boolean";
	public static final String XSD_DATATYPE_DATETIME = "http://www.w3.org/2001/XMLSchema#datetime";
	public static final String XSD_DATATYPE_INT = "http://www.w3.org/2001/XMLSchema#int";
	public static final String XSD_DATATYPE_DECIMAL = "http://www.w3.org/2001/XMLSchema#decimal";
	public static final String XSD_DATATYPE_FLOAT = "http://www.w3.org/2001/XMLSchema#float";

	// OSLC
	public static final String OSLC_VERSION = "2.0"; 
	public static final String HDR_OSLC_VERSION = "OSLC-Core-Version"; 
	public static final String OSLC_CORE_DOMAIN = "http://open-services.net/ns/core#core";  
	public static final String OSLC_CORE_RESOURCE = "http://open-services.net/ns/core#Resource";  
	public static final String OSLC_DEFAULT = "http://open-services.net/ns/core#default";


	// OSLC CM 2.0
	static String OSLC_CM_V2 = "http://open-services.net/ns/cm#";
	public static final String CM_CLOSE_DATE_PROP 	= OSLC_CM_V2 + "closeDate";
	public static final String CM_STATUS_PROP 		= OSLC_CM_V2 + "status";
	public static final String CM_CLOSED_PROP 		= OSLC_CM_V2 + "closed";
	public static final String CM_INPROGRESS_PROP 	= OSLC_CM_V2 + "inprogress";
	public static final String CM_FIXED_PROP 		= OSLC_CM_V2 + "fixed";
	public static final String CM_APPROVED_PROP 	= OSLC_CM_V2 + "approved";
	public static final String CM_REVIEWED_PROP 	= OSLC_CM_V2 + "reviewed";
	public static final String CM_VERIFIED_PROP 	= OSLC_CM_V2 + "verified";

	
	// content types
	static final String CT_RDF_XML = "application/rdf+xml"; 
	static final String CT_APP_N3 = "application/n3"; 
	static final String CT_TEXT_N3 = "text/n3"; 
	static final String CT_TEXT_TURTLE = "text/turtle"; 
	static final String CT_X_TURTLE = "application/x-turtle"; 
	static final String CT_X_TRIG = "application/x-trig"; 
	static final String CT_X_TRIX = "application/x-trix"; 
	static final String CT_APP_N_TRIPLES = "application/n-triples"; 
	static final String CT_XML = "application/xml"; 
	static final String CT_HTML = "text/html"; 
	static final String CT_XHTML = "application/xhtml+xml"; 
	static final String CT_TEXT_PLAIN = "text/plain"; 
	static final String CT_JSON = "application/json"; 
	static final String CT_OSLC_COMPACT = "application/x-oslc-compact+xml"; 
	static final String CT_IMAGE_PNG = "image/png"; 
	
	// HTTP Status Codes
	static final int SC_OK = 200; 
	static final int SC_CREATED = 201;
	static final int SC_BAD = 400;
	static final int SC_FORBIDDEN = 403;
	static final int SC_NOT_FOUND = 404; 
	static final int SC_NOT_ACCEPTABLE = 406;
	static final int SC_CONFLICT = 409; 
	static final int SC_PRECONDITION_FAILED = 412;
	static final int SC_UNSUPPORTED_MEDIA_TYPE = 415;
	static final int SC_INTERNAL_ERROR = 500;
	
	// HTTP Headers
	public static final String HDR_ACCEPT = "Accept";  
	public static final String HDR_CONTENT_TYPE = "Content-Type"; 
	public static final String HDR_ETAG = "ETag"; 
	public static final String HDR_LAST_MODIFIED = "Last-Modified"; 
	public static final String HDR_LOCATION = "Location";  
	public static final String HDR_IF_MATCH = "If-Match";  
	public static final String HDR_IF_UNMODIFIED_SINCE = "If-Unmodified-Since";
	public static final String HDR_SLUG = "Slug";
	
	// MISC
	public static final String TEXT_ENCODING = "UTF-8"; 
	public static final String SPARQL = "SPARQL"; 
	public static final String OSLC = "OSLC"; 

	// delegated UI
	public static final String POST_MESSAGE_FRAGMENT = "#oslc-core-postMessage-1.0";  
	public static final String WINDOW_NAME_FRAGMENT = "#oslc-core-windowName-1.0";  


	/**
	 * Properties and Types
	 */

	// oslc
	public static final String OSLC_NAMESPACE = "http://open-services.net/ns/core#";
	public static final String OSLC_PREFIX = "oslc";
	public static final String OSLC_XMLS_DECL = "\txmlns:" + OSLC_PREFIX + "=\"" + OSLC_NAMESPACE + "\"\n";

	// oslc types 
	public static final String OSLC_TYPE_TERM_SERVICEPROVIDERCATALOG = "ServiceProviderCatalog";
	public static final String OSLC_TYPE_PTERM_SERVICEPROVIDERCATALOG = OSLC_PREFIX + ':' + OSLC_TYPE_TERM_SERVICEPROVIDERCATALOG;
	public static final String OSLC_TYPE_SERVICEPROVIDERCATALOG = OSLC_NAMESPACE + OSLC_TYPE_TERM_SERVICEPROVIDERCATALOG;
	public static final String OSLC_TYPE_TERM_SERVICEPROVIDER = "ServiceProvider";
	public static final String OSLC_TYPE_PTERM_SERVICEPROVIDER = OSLC_PREFIX + ':' + OSLC_TYPE_TERM_SERVICEPROVIDER;
	public static final String OSLC_TYPE_SERVICEPROVIDER = OSLC_NAMESPACE + OSLC_TYPE_TERM_SERVICEPROVIDER;
	public static final String OSLC_TYPE_TERM_ERROR = "Error";
	public static final String OSLC_TYPE_PTERM_ERROR = OSLC_PREFIX + ':' + OSLC_TYPE_TERM_ERROR;
	public static final String OSLC_TYPE_ERROR = OSLC_NAMESPACE + OSLC_TYPE_TERM_ERROR;
	public static final String OSLC_TYPE_TERM_EXTENDEDERROR = "ExtendedError";
	public static final String OSLC_TYPE_PTERM_EXTENDEDERROR = OSLC_PREFIX + ':' + OSLC_TYPE_TERM_EXTENDEDERROR;
	public static final String OSLC_TYPE_EXTENDEDERROR = OSLC_NAMESPACE + OSLC_TYPE_TERM_EXTENDEDERROR;
	public static final String OSLC_TYPE_TERM_PUBLISHER = "Publisher";
	public static final String OSLC_TYPE_PTERM_PUBLISHER = OSLC_PREFIX + ':' + OSLC_TYPE_TERM_PUBLISHER;
	public static final String OSLC_TYPE_PUBLISHER = OSLC_NAMESPACE + OSLC_TYPE_TERM_PUBLISHER;
	public static final String OSLC_TYPE_TERM_PREFIXDEFINITION = "PrefixDefinition";
	public static final String OSLC_TYPE_PTERM_PREFIXDEFINITION = OSLC_PREFIX + ':' + OSLC_TYPE_TERM_PREFIXDEFINITION;
	public static final String OSLC_TYPE_PREFIXDEFINITION = OSLC_NAMESPACE + OSLC_TYPE_TERM_PREFIXDEFINITION;
	public static final String OSLC_TYPE_TERM_SERVICE = "Service";
	public static final String OSLC_TYPE_PTERM_SERVICE = OSLC_PREFIX + ':' + OSLC_TYPE_TERM_SERVICE;
	public static final String OSLC_TYPE_SERVICE = OSLC_NAMESPACE + OSLC_TYPE_TERM_SERVICE;
	public static final String OSLC_TYPE_TERM_QUERYCAPABILITY = "QueryCapability";
	public static final String OSLC_TYPE_PTERM_QUERYCAPABILITY = OSLC_PREFIX + ':' + OSLC_TYPE_TERM_QUERYCAPABILITY;
	public static final String OSLC_TYPE_QUERYCAPABILITY = OSLC_NAMESPACE + OSLC_TYPE_TERM_QUERYCAPABILITY;
	public static final String OSLC_TYPE_TERM_RESOURCESHAPE = "ResourceShape";
	public static final String OSLC_TYPE_PTERM_RESOURCESHAPE = OSLC_PREFIX + ':' + OSLC_TYPE_TERM_RESOURCESHAPE;
	public static final String OSLC_TYPE_RESOURCESHAPE = OSLC_NAMESPACE + OSLC_TYPE_TERM_RESOURCESHAPE;
	public static final String OSLC_TYPE_TERM_CREATIONFACTORY = "CreationFactory";
	public static final String OSLC_TYPE_PTERM_CREATIONFACTORY = OSLC_PREFIX + ':' + OSLC_TYPE_TERM_CREATIONFACTORY;
	public static final String OSLC_TYPE_CREATIONFACTORY = OSLC_NAMESPACE + OSLC_TYPE_TERM_CREATIONFACTORY;
	public static final String OSLC_TYPE_TERM_SELECTIONDIALOG = "SelectionDialog";
	public static final String OSLC_TYPE_PTERM_SELECTIONDIALOG = OSLC_PREFIX + ':' + OSLC_TYPE_TERM_SELECTIONDIALOG;
	public static final String OSLC_TYPE_SELECTIONDIALOG = OSLC_NAMESPACE + OSLC_TYPE_TERM_SELECTIONDIALOG;
	public static final String OSLC_TYPE_TERM_CREATIONDIALOG = "CreationDialog";
	public static final String OSLC_TYPE_PTERM_CREATIONDIALOG = OSLC_PREFIX + ':' + OSLC_TYPE_TERM_CREATIONDIALOG;
	public static final String OSLC_TYPE_CREATIONDIALOG = OSLC_NAMESPACE + OSLC_TYPE_TERM_CREATIONDIALOG;
	public static final String OSLC_TYPE_TERM_DIALOG = "Dialog";
	public static final String OSLC_TYPE_PTERM_DIALOG = OSLC_PREFIX + ':' + OSLC_TYPE_TERM_DIALOG;
	public static final String OSLC_TYPE_DIALOG = OSLC_NAMESPACE + OSLC_TYPE_TERM_DIALOG;
	public static final String OSLC_TYPE_TERM_OAUTHCONFIGURATION = "OAuthConfiguration";
	public static final String OSLC_TYPE_PTERM_OAUTHCONFIGURATION = OSLC_PREFIX + ':' + OSLC_TYPE_TERM_OAUTHCONFIGURATION;
	public static final String OSLC_TYPE_OAUTHCONFIGURATION = OSLC_NAMESPACE + OSLC_TYPE_TERM_OAUTHCONFIGURATION;
	public static final String OSLC_TERM_PUBLISHER = "publisher";
	public static final String OSLC_PTERM_PUBLISHER = OSLC_PREFIX + ':' + OSLC_TERM_PUBLISHER;
	public static final String OSLC_PUBLISHER = OSLC_NAMESPACE + OSLC_TERM_PUBLISHER;
	public static final String OSLC_TERM_DESCRIBES = "describes";
	public static final String OSLC_PTERM_DESCRIBES = OSLC_PREFIX + ':' + OSLC_TERM_DESCRIBES;
	public static final String OSLC_DESCRIBES = OSLC_NAMESPACE + OSLC_TERM_DESCRIBES;
	
	// oslc properties 
	public static final String OSLC_TERM_PERVICEPROVIDERCATALOG = "perviceProviderCatalog";
	public static final String OSLC_PTERM_PERVICEPROVIDERCATALOG = OSLC_PREFIX + ':' + OSLC_TERM_PERVICEPROVIDERCATALOG;
	public static final String OSLC_PERVICEPROVIDERCATALOG = OSLC_NAMESPACE + OSLC_TERM_PERVICEPROVIDERCATALOG;
	public static final String OSLC_TERM_SERVICEPROVIDER = "serviceProvider";
	public static final String OSLC_PTERM_SERVICEPROVIDER = OSLC_PREFIX + ':' + OSLC_TERM_SERVICEPROVIDER;
	public static final String OSLC_SERVICEPROVIDER = OSLC_NAMESPACE + OSLC_TERM_SERVICEPROVIDER;
	public static final String OSLC_TERM_INSTANCESHAPE = "instanceShape";
	public static final String OSLC_PTERM_INSTANCESHAPE = OSLC_PREFIX + ':' + OSLC_TERM_INSTANCESHAPE;
	public static final String OSLC_INSTANCESHAPE = OSLC_NAMESPACE + OSLC_TERM_INSTANCESHAPE;
	public static final String OSLC_TERM_RESPONSEINFO = "responseInfo";
	public static final String OSLC_PTERM_RESPONSEINFO = OSLC_PREFIX + ':' + OSLC_TERM_RESPONSEINFO;
	public static final String OSLC_RESPONSEINFO = OSLC_NAMESPACE + OSLC_TERM_RESPONSEINFO;
	public static final String OSLC_TERM_ICON = "icon";
	public static final String OSLC_PTERM_ICON = OSLC_PREFIX + ':' + OSLC_TERM_ICON;
	public static final String OSLC_ICON = OSLC_NAMESPACE + OSLC_TERM_ICON;
	public static final String OSLC_TERM_PREFIXDEFINITION = "prefixDefinition";
	public static final String OSLC_PTERM_PREFIXDEFINITION = OSLC_PREFIX + ':' + OSLC_TERM_PREFIXDEFINITION;
	public static final String OSLC_PREFIXDEFINITION = OSLC_NAMESPACE + OSLC_TERM_PREFIXDEFINITION;
	public static final String OSLC_TERM_PREFIX = "prefix";
	public static final String OSLC_PTERM_PREFIX = OSLC_PREFIX + ':' + OSLC_TERM_PREFIX;
	public static final String OSLC_PREFIX_PROP = OSLC_NAMESPACE + OSLC_TERM_PREFIX;
	public static final String OSLC_TERM_PREFIXBASE = "prefixBase";
	public static final String OSLC_PTERM_PREFIXBASE = OSLC_PREFIX + ':' + OSLC_TERM_PREFIXBASE;
	public static final String OSLC_PREFIXBASE = OSLC_NAMESPACE + OSLC_TERM_PREFIXBASE;
	public static final String OSLC_TERM_SERVICE = "service";
	public static final String OSLC_PTERM_SERVICE = OSLC_PREFIX + ':' + OSLC_TERM_SERVICE;
	public static final String OSLC_SERVICE = OSLC_NAMESPACE + OSLC_TERM_SERVICE;
	public static final String OSLC_TERM_DOMAIN = "domain";
	public static final String OSLC_PTERM_DOMAIN = OSLC_PREFIX + ':' + OSLC_TERM_DOMAIN;
	public static final String OSLC_DOMAIN = OSLC_NAMESPACE + OSLC_TERM_DOMAIN;
	public static final String OSLC_TERM_QUERYCAPABILITY = "queryCapability";
	public static final String OSLC_PTERM_QUERYCAPABILITY = OSLC_PREFIX + ':' + OSLC_TERM_QUERYCAPABILITY;
	public static final String OSLC_QUERYCAPABILITY = OSLC_NAMESPACE + OSLC_TERM_QUERYCAPABILITY;
	public static final String OSLC_TERM_LABEL = "label";
	public static final String OSLC_PTERM_LABEL = OSLC_PREFIX + ':' + OSLC_TERM_LABEL;
	public static final String OSLC_LABEL = OSLC_NAMESPACE + OSLC_TERM_LABEL;
	public static final String OSLC_TERM_QUERYBASE = "queryBase";
	public static final String OSLC_PTERM_QUERYBASE = OSLC_PREFIX + ':' + OSLC_TERM_QUERYBASE;
	public static final String OSLC_QUERYBASE = OSLC_NAMESPACE + OSLC_TERM_QUERYBASE;
	public static final String OSLC_TERM_CREATION = "creation";
	public static final String OSLC_PTERM_CREATION = OSLC_PREFIX + ':' + OSLC_TERM_CREATION;
	public static final String OSLC_CREATION = OSLC_NAMESPACE + OSLC_TERM_CREATION;
	public static final String OSLC_TERM_RESOURCESHAPE = "resourceShape";
	public static final String OSLC_PTERM_RESOURCESHAPE = OSLC_PREFIX + ':' + OSLC_TERM_RESOURCESHAPE;
	public static final String OSLC_RESOURCESHAPE = OSLC_NAMESPACE + OSLC_TERM_RESOURCESHAPE;
	public static final String OSLC_TERM_CREATIONFACTORY = "creationFactory";
	public static final String OSLC_PTERM_CREATIONFACTORY = OSLC_PREFIX + ':' + OSLC_TERM_CREATIONFACTORY;
	public static final String OSLC_CREATIONFACTORY = OSLC_NAMESPACE + OSLC_TERM_CREATIONFACTORY;
	public static final String OSLC_TERM_RESOURCETYPE = "resourceType";
	public static final String OSLC_PTERM_RESOURCETYPE = OSLC_PREFIX + ':' + OSLC_TERM_RESOURCETYPE;
	public static final String OSLC_RESOURCETYPE = OSLC_NAMESPACE + OSLC_TERM_RESOURCETYPE;
	public static final String OSLC_TERM_USAGE = "usage";
	public static final String OSLC_PTERM_USAGE = OSLC_PREFIX + ':' + OSLC_TERM_USAGE;
	public static final String OSLC_USAGE = OSLC_NAMESPACE + OSLC_TERM_USAGE;
	public static final String OSLC_TERM_SELECTIONDIALOG = "selectionDialog";
	public static final String OSLC_PTERM_SELECTIONDIALOG = OSLC_PREFIX + ':' + OSLC_TERM_SELECTIONDIALOG;
	public static final String OSLC_SELECTIONDIALOG = OSLC_NAMESPACE + OSLC_TERM_SELECTIONDIALOG;
	public static final String OSLC_TERM_CREATIONDIALOG = "creationDialog";
	public static final String OSLC_PTERM_CREATIONDIALOG = OSLC_PREFIX + ':' + OSLC_TERM_CREATIONDIALOG;
	public static final String OSLC_CREATIONDIALOG = OSLC_NAMESPACE + OSLC_TERM_CREATIONDIALOG;
	public static final String OSLC_TERM_DIALOG = "dialog";
	public static final String OSLC_PTERM_DIALOG = OSLC_PREFIX + ':' + OSLC_TERM_DIALOG;
	public static final String OSLC_DIALOG = OSLC_NAMESPACE + OSLC_TERM_DIALOG;
	public static final String OSLC_TERM_HINTWIDTH = "hintWidth";
	public static final String OSLC_PTERM_HINTWIDTH = OSLC_PREFIX + ':' + OSLC_TERM_HINTWIDTH;
	public static final String OSLC_HINTWIDTH = OSLC_NAMESPACE + OSLC_TERM_HINTWIDTH;
	public static final String OSLC_TERM_HINTHEIGHT = "hintHeight";
	public static final String OSLC_PTERM_HINTHEIGHT = OSLC_PREFIX + ':' + OSLC_TERM_HINTHEIGHT;
	public static final String OSLC_HINTHEIGHT = OSLC_NAMESPACE + OSLC_TERM_HINTHEIGHT;
	public static final String OSLC_TERM_SHORTTITLE = "shortTitle";
	public static final String OSLC_PTERM_SHORTTITLE = OSLC_PREFIX + ':' + OSLC_TERM_SHORTTITLE;
	public static final String OSLC_SHORTTITLE = OSLC_NAMESPACE + OSLC_TERM_SHORTTITLE;
	public static final String OSLC_TERM_DETAILS = "details";
	public static final String OSLC_PTERM_DETAILS = OSLC_PREFIX + ':' + OSLC_TERM_DETAILS;
	public static final String OSLC_DETAILS = OSLC_NAMESPACE + OSLC_TERM_DETAILS;
	public static final String OSLC_TERM_OAUTHCONFIGURATION = "oauthConfiguration";
	public static final String OSLC_PTERM_OAUTHCONFIGURATION = OSLC_PREFIX + ':' + OSLC_TERM_OAUTHCONFIGURATION;
	public static final String OSLC_OAUTHCONFIGURATION = OSLC_NAMESPACE + OSLC_TERM_OAUTHCONFIGURATION;
	public static final String OSLC_TERM_OAUTHREQUESTTOKENURI = "oauthRequestTokenURI";
	public static final String OSLC_PTERM_OAUTHREQUESTTOKENURI = OSLC_PREFIX + ':' + OSLC_TERM_OAUTHREQUESTTOKENURI;
	public static final String OSLC_OAUTHREQUESTTOKENURI = OSLC_NAMESPACE + OSLC_TERM_OAUTHREQUESTTOKENURI;
	public static final String OSLC_TERM_AUTHORIZATIONURI = "authorizationURI";
	public static final String OSLC_PTERM_AUTHORIZATIONURI = OSLC_PREFIX + ':' + OSLC_TERM_AUTHORIZATIONURI;
	public static final String OSLC_AUTHORIZATIONURI = OSLC_NAMESPACE + OSLC_TERM_AUTHORIZATIONURI;
	public static final String OSLC_TERM_OAUTHACCESSTOKENURI = "oauthAccessTokenURI";
	public static final String OSLC_PTERM_OAUTHACCESSTOKENURI = OSLC_PREFIX + ':' + OSLC_TERM_OAUTHACCESSTOKENURI;
	public static final String OSLC_OAUTHACCESSTOKENURI = OSLC_NAMESPACE + OSLC_TERM_OAUTHACCESSTOKENURI;

	// dcterms
	public static final String DCTERMS_NAMESPACE = "http://purl.org/dc/terms/";
	public static final String DCTERMS_PREFIX = "dcterms";
	public static final String DCTERMS_XMLS_DECL = "\txmlns:" + DCTERMS_PREFIX + "=\"" + DCTERMS_NAMESPACE + "\"\n";

	// dcterms types 

	// dcterms properties 
	public static final String DCTERMS_TERM_TITLE = "title";
	public static final String DCTERMS_PTERM_TITLE = DCTERMS_PREFIX + ':' + DCTERMS_TERM_TITLE;
	public static final String DCTERMS_TITLE = DCTERMS_NAMESPACE + DCTERMS_TERM_TITLE;
	public static final String DCTERMS_TERM_DESCRIPTION = "description";
	public static final String DCTERMS_PTERM_DESCRIPTION = DCTERMS_PREFIX + ':' + DCTERMS_TERM_DESCRIPTION;
	public static final String DCTERMS_DESCRIPTION = DCTERMS_NAMESPACE + DCTERMS_TERM_DESCRIPTION;
	public static final String DCTERMS_TERM_PUBLISHER = "publisher";
	public static final String DCTERMS_PTERM_PUBLISHER = DCTERMS_PREFIX + ':' + DCTERMS_TERM_PUBLISHER;
	public static final String DCTERMS_PUBLISHER = DCTERMS_NAMESPACE + DCTERMS_TERM_PUBLISHER;
	public static final String DCTERMS_TERM_IDENTIFIER = "identifier";
	public static final String DCTERMS_PTERM_IDENTIFIER = DCTERMS_PREFIX + ':' + DCTERMS_TERM_IDENTIFIER;
	public static final String DCTERMS_IDENTIFIER = DCTERMS_NAMESPACE + DCTERMS_TERM_IDENTIFIER;
	public static final String DCTERMS_TERM_RELATION = "relation";
	public static final String DCTERMS_PTERM_RELATION = DCTERMS_PREFIX + ':' + DCTERMS_TERM_RELATION;
	public static final String DCTERMS_RELATION = DCTERMS_NAMESPACE + DCTERMS_TERM_RELATION;
	public static final String DCTERMS_TERM_SOURCE = "source";
	public static final String DCTERMS_PTERM_SOURCE = DCTERMS_PREFIX + ':' + DCTERMS_TERM_SOURCE;
	public static final String DCTERMS_SOURCE = DCTERMS_NAMESPACE + DCTERMS_TERM_SOURCE;
	public static final String DCTERMS_TERM_ISVERSIONOF = "isVersionOf";
	public static final String DCTERMS_PTERM_ISVERSIONOF = DCTERMS_PREFIX + ':' + DCTERMS_TERM_ISVERSIONOF;
	public static final String DCTERMS_ISVERSIONOF = DCTERMS_NAMESPACE + DCTERMS_TERM_ISVERSIONOF;
	public static final String DCTERMS_TERM_HASVERSION = "hasVersion";
	public static final String DCTERMS_PTERM_HASVERSION = DCTERMS_PREFIX + ':' + DCTERMS_TERM_HASVERSION;
	public static final String DCTERMS_HASVERSION = DCTERMS_NAMESPACE + DCTERMS_TERM_HASVERSION;
	public static final String DCTERMS_TERM_CREATED = "created";
	public static final String DCTERMS_PTERM_CREATED = DCTERMS_PREFIX + ':' + DCTERMS_TERM_CREATED;
	public static final String DCTERMS_CREATED = DCTERMS_NAMESPACE + DCTERMS_TERM_CREATED;
	public static final String DCTERMS_TERM_MODIFIED = "modified";
	public static final String DCTERMS_PTERM_MODIFIED = DCTERMS_PREFIX + ':' + DCTERMS_TERM_MODIFIED;
	public static final String DCTERMS_MODIFIED = DCTERMS_NAMESPACE + DCTERMS_TERM_MODIFIED;
	public static final String DCTERMS_TERM_CONTRIBUTOR = "contributor";
	public static final String DCTERMS_PTERM_CONTRIBUTOR = DCTERMS_PREFIX + ':' + DCTERMS_TERM_CONTRIBUTOR;
	public static final String DCTERMS_CONTRIBUTOR = DCTERMS_NAMESPACE + DCTERMS_TERM_CONTRIBUTOR;
	public static final String DCTERMS_TERM_CREATOR = "creator";
	public static final String DCTERMS_PTERM_CREATOR = DCTERMS_PREFIX + ':' + DCTERMS_TERM_CREATOR;
	public static final String DCTERMS_CREATOR = DCTERMS_NAMESPACE + DCTERMS_TERM_CREATOR;
	public static final String DCTERMS_TERM_SUBJECT = "subject";
	public static final String DCTERMS_PTERM_SUBJECT = DCTERMS_PREFIX + ':' + DCTERMS_TERM_SUBJECT;
	public static final String DCTERMS_SUBJECT = DCTERMS_NAMESPACE + DCTERMS_TERM_SUBJECT;
	public static final String DCTERMS_TERM_TYPE = "type";
	public static final String DCTERMS_PTERM_TYPE = DCTERMS_PREFIX + ':' + DCTERMS_TERM_TYPE;
	public static final String DCTERMS_TYPE = DCTERMS_NAMESPACE + DCTERMS_TERM_TYPE;
	public static final String DCTERMS_TERM_REPLACES = "replaces";
	public static final String DCTERMS_PTERM_REPLACES = DCTERMS_PREFIX + ':' + DCTERMS_TERM_REPLACES;
	public static final String DCTERMS_REPLACES = DCTERMS_NAMESPACE + DCTERMS_TERM_REPLACES;
	public static final String DCTERMS_TERM_HASPART = "hasPart";
	public static final String DCTERMS_PTERM_HASPART = DCTERMS_PREFIX + ':' + DCTERMS_TERM_HASPART;
	public static final String DCTERMS_HASPART = DCTERMS_NAMESPACE + DCTERMS_TERM_HASPART;


	// rdf
	public static final String RDF_NAMESPACE = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
	public static final String RDF_PREFIX = "rdf";
	public static final String RDF_XMLS_DECL = "\txmlns:" + RDF_PREFIX + "=\"" + RDF_NAMESPACE + "\"\n";

	// rdf types 
	public static final String RDF_TYPE_TERM_RDF = "RDF";
	public static final String RDF_TYPE_PTERM_RDF = RDF_PREFIX + ':' + RDF_TYPE_TERM_RDF;
	public static final String RDF_TYPE_RDF = RDF_NAMESPACE + RDF_TYPE_TERM_RDF;
	public static final String RDF_TYPE_TERM_SEQ = "Seq";
	public static final String RDF_TYPE_PTERM_SEQ = RDF_PREFIX + ':' + RDF_TYPE_TERM_SEQ;
	public static final String RDF_TYPE_SEQ = RDF_NAMESPACE + RDF_TYPE_TERM_SEQ;
	public static final String RDF_TYPE_TERM_BAG = "Bag";
	public static final String RDF_TYPE_PTERM_BAG = RDF_PREFIX + ':' + RDF_TYPE_TERM_BAG;
	public static final String RDF_TYPE_BAG = RDF_NAMESPACE + RDF_TYPE_TERM_BAG;
	public static final String RDF_TYPE_TERM_STATEMENT = "Statement";
	public static final String RDF_TYPE_PTERM_STATEMENT = RDF_PREFIX + ':' + RDF_TYPE_TERM_STATEMENT;
	public static final String RDF_TYPE_STATEMENT = RDF_NAMESPACE + RDF_TYPE_TERM_STATEMENT;
	public static final String RDF_TYPE_TERM_DESCRIPTION = "Description";
	public static final String RDF_TYPE_PTERM_DESCRIPTION = RDF_PREFIX + ':' + RDF_TYPE_TERM_DESCRIPTION;
	public static final String RDF_TYPE_DESCRIPTION = RDF_NAMESPACE + RDF_TYPE_TERM_DESCRIPTION;
	public static final String RDF_TYPE_TERM_ID = "ID";
	public static final String RDF_TYPE_PTERM_ID = RDF_PREFIX + ':' + RDF_TYPE_TERM_ID;
	public static final String RDF_TYPE_ID = RDF_NAMESPACE + RDF_TYPE_TERM_ID;

	// rdf properties 
	public static final String RDF_TERM_TYPE = "type";
	public static final String RDF_PTERM_TYPE = RDF_PREFIX + ':' + RDF_TERM_TYPE;
	public static final String RDF_TYPE = RDF_NAMESPACE + RDF_TERM_TYPE;
	public static final String RDF_TERM_SUBJECT = "subject";
	public static final String RDF_PTERM_SUBJECT = RDF_PREFIX + ':' + RDF_TERM_SUBJECT;
	public static final String RDF_SUBJECT = RDF_NAMESPACE + RDF_TERM_SUBJECT;
	public static final String RDF_TERM_PREDICATE = "predicate";
	public static final String RDF_PTERM_PREDICATE = RDF_PREFIX + ':' + RDF_TERM_PREDICATE;
	public static final String RDF_PREDICATE = RDF_NAMESPACE + RDF_TERM_PREDICATE;
	public static final String RDF_TERM_OBJECT = "object";
	public static final String RDF_PTERM_OBJECT = RDF_PREFIX + ':' + RDF_TERM_OBJECT;
	public static final String RDF_OBJECT = RDF_NAMESPACE + RDF_TERM_OBJECT;
	public static final String RDF_TERM_RESOURCE = "resource";
	public static final String RDF_PTERM_RESOURCE = RDF_PREFIX + ':' + RDF_TERM_RESOURCE;
	public static final String RDF_RESOURCE = RDF_NAMESPACE + RDF_TERM_RESOURCE;
	public static final String RDF_TERM_ABOUT = "about";
	public static final String RDF_PTERM_ABOUT = RDF_PREFIX + ':' + RDF_TERM_ABOUT;
	public static final String RDF_ABOUT = RDF_NAMESPACE + RDF_TERM_ABOUT;
	public static final String RDF_TERM_DATATYPE = "datatype";
	public static final String RDF_PTERM_DATATYPE = RDF_PREFIX + ':' + RDF_TERM_DATATYPE;
	public static final String RDF_DATATYPE = RDF_NAMESPACE + RDF_TERM_DATATYPE;
	public static final String RDF_TERM_LI = "li";
	public static final String RDF_PTERM_LI = RDF_PREFIX + ':' + RDF_TERM_LI;
	public static final String RDF_LI = RDF_NAMESPACE + RDF_TERM_LI;
	public static final String RDF_TERM_SEQ = "seq";
	public static final String RDF_PTERM_SEQ = RDF_PREFIX + ':' + RDF_TERM_SEQ;
	public static final String RDF_SEQ = RDF_NAMESPACE + RDF_TERM_SEQ;
	public static final String RDF_TERM_BAG = "bag";
	public static final String RDF_PTERM_BAG = RDF_PREFIX + ':' + RDF_TERM_BAG;
	public static final String RDF_BAG = RDF_NAMESPACE + RDF_TERM_BAG;
	public static final String RDF_TERM_STATEMENT = "statement";
	public static final String RDF_PTERM_STATEMENT = RDF_PREFIX + ':' + RDF_TERM_STATEMENT;
	public static final String RDF_STATEMENT = RDF_NAMESPACE + RDF_TERM_STATEMENT;
	// rdfs
	public static final String RDFS_NAMESPACE = "http://www.w3.org/2000/01/rdf-schema#";
	public static final String RDFS_PREFIX = "rdfs";
	public static final String RDFS_XMLS_DECL = "\txmlns:" + RDFS_PREFIX + "=\"" + RDFS_NAMESPACE + "\"\n";

	// rdfs types 

	// rdfs properties 
	public static final String RDFS_TERM_MEMBER = "member";
	public static final String RDFS_PTERM_MEMBER = RDFS_PREFIX + ':' + RDFS_TERM_MEMBER;
	public static final String RDFS_MEMBER = RDFS_NAMESPACE + RDFS_TERM_MEMBER;
	public static final String RDFS_TERM_LABEL = "label";
	public static final String RDFS_PTERM_LABEL = RDFS_PREFIX + ':' + RDFS_TERM_LABEL;
	public static final String RDFS_LABEL = RDFS_NAMESPACE + RDFS_TERM_LABEL;
	public static final String RDFS_TERM_COMMENTS = "comments";
	public static final String RDFS_PTERM_COMMENTS = RDFS_PREFIX + ':' + RDFS_TERM_COMMENTS;
	public static final String RDFS_COMMENTS = RDFS_NAMESPACE + RDFS_TERM_COMMENTS;


	// foaf
	public static final String FOAF_NAMESPACE = "http://http://xmlns.com/foaf/0.1/";
	public static final String FOAF_PREFIX = "foaf";
	public static final String FOAF_XMLS_DECL = "\txmlns:" + FOAF_PREFIX + "=\"" + FOAF_NAMESPACE + "\"\n";

	// foaf types 
	public static final String FOAF_TYPE_TERM_PERSON = "Person";
	public static final String FOAF_TYPE_PTERM_PERSON = FOAF_PREFIX + ':' + FOAF_TYPE_TERM_PERSON;
	public static final String FOAF_TYPE_PERSON = FOAF_NAMESPACE + FOAF_TYPE_TERM_PERSON;

	// foaf properties 
	public static final String FOAF_TERM_NAME = "name";
	public static final String FOAF_PTERM_NAME = FOAF_PREFIX + ':' + FOAF_TERM_NAME;
	public static final String FOAF_NAME = FOAF_NAMESPACE + FOAF_TERM_NAME;

}
