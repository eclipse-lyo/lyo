/*******************************************************************************
 * Copyright (c) 2011, 2014 IBM Corporation.
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
 *    Steve Speicher - initial API and implementation
 *    Michael Fiedler - add AM resource types
 *    Samuel Padgett  - deprecate constants for non-existent terms
 *******************************************************************************/

package org.eclipse.lyo.oslc4j.client;

/**
 * General OSLC constants
 *
 */
public interface OSLCConstants {
	String RFC_DATE_FORMAT = "yyyy-MM-dd'T'h:m:ss.S'Z'";

	String DC = "http://purl.org/dc/terms/";
	String DCTERMS ="dcterms:";
	String RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
	String RDFS = "http://www.w3.org/2000/01/rdf-schema#";
	String ATOM = "http://www.w3.org/2005/Atom";
	String OSLC_V2    = "http://open-services.net/ns/core#";
	String CORE_DEFAULT = "http://open-services.net/ns/core#default";
	String OSLC_CM_V2 = "http://open-services.net/ns/cm#";
	String OSLC_AM_V2 = "http://open-services.net/ns/am#";
	String OSLC_ASSET_V2 = "http://open-services.net/ns/asset#";
	String OSLC_QM_V2 = "http://open-services.net/ns/qm#";
	String OSLC_RM_V2 = "http://open-services.net/ns/rm#";
	String OSLC_AUTO = "http://open-services.net/ns/auto#";

	// Version 1.0 namespace definitions
	String OSLC_DISC = "http://open-services.net/xmlns/discovery/1.0/";
	String OSLC_CM   = "http://open-services.net/xmlns/cm/1.0/";
	String OSLC_QM   = "http://open-services.net/xmlns/qm/1.0/";
	String OSLC_RM   = "http://open-services.net/xmlns/rm/1.0/";


	//--------------------------------------------------------------------------
	// Content-types for Accept header requests
	// Standard headers:
	String CT_XML = "application/xml";
	String CT_RDF = "application/rdf+xml";
	String CT_JSON = "application/json";
	String CT_ATOM = "application/atom+xml";

	// Version 1 headers:
	String CT_CR_XML = "application/x-oslc-cm-change-request+xml";
	String CT_CR_JSON = "application/x-oslc-cm-change-request+json";
	String CT_CR_QUERY = "application/x-oslc-cm-change-request+xml";
	String CT_DISC_CAT_XML = "application/x-oslc-disc-service-provider-catalog+xml";
	String CT_DISC_DESC_XML = "application/x-oslc-cm-service-description+xml";

	// Version 2 headers:
	String OSLC_CORE_VERSION = "OSLC-Core-Version";
	String ETAG = "Etag";

	String POST = "POST";
	String SSL = "SSL";

	String JENA_RDF_XML = "RDF/XML";

	//--------------------------------------------------------------------------
	// Property URIs

	// OSLC Core
	String SERVICE_PROVIDER_PROP = OSLC_V2 + "serviceProvider";
	String SERVICE_PROVIDER_TYPE = OSLC_V2 + "ServiceProvider";
	String SERVICE_PROVIDER_CATALOG_PROP = OSLC_V2 + "serviceProviderCatalog";
	String SERVICE_PROVIDER_CATALOG_TYPE = OSLC_V2 + "ServiceProviderCatalog";
	String CREATION_PROP 		= OSLC_V2 + "creation";
	String QUERY_CAPABILITY_PROP = OSLC_V2 + "QueryCapability";
	String QUERY_BASE_PROP 		= OSLC_V2 + "queryBase";
	String RESP_INFO_TYPE 		= OSLC_V2 + "ResponseInfo";
	String SERVICE_PROP 		= OSLC_V2 + "service";
	String DISCUSSION_PROP 		= OSLC_V2 + "discussion";
	String INST_SHAPE_PROP 		= OSLC_V2 + "instanceShape";
	String USAGE_PROP		 	= OSLC_V2 + "usage";
	String USAGE_DEFAULT_URI 	= OSLC_V2 + "default";
	String TOTAL_COUNT_PROP	 	= OSLC_V2 + "totalCount";
	String RESOURCE_TYPE_PROP   = OSLC_V2 + "resourceType";
	String RESOURCE_SHAPE_PROP  = OSLC_V2 + "resourceShape";
	String DESCRIPTION_PROP 	= OSLC_V2 + "Description";
	// OSLC CM 2.0
	String CM_CHANGE_REQUEST_TYPE = OSLC_CM_V2 + "ChangeRequest";
	String CM_CLOSE_DATE_PROP 	= OSLC_CM_V2 + "closeDate";
	String CM_STATUS_PROP 		= OSLC_CM_V2 + "status";
	String CM_CLOSED_PROP 		= OSLC_CM_V2 + "closed";
	String CM_INPROGRESS_PROP 	= OSLC_CM_V2 + "inprogress";
	String CM_FIXED_PROP 		= OSLC_CM_V2 + "fixed";
	String CM_APPROVED_PROP 	= OSLC_CM_V2 + "approved";
	String CM_REVIEWED_PROP 	= OSLC_CM_V2 + "reviewed";
	String CM_VERIFIED_PROP 	= OSLC_CM_V2 + "verified";

	// OSLC QM 2.0
	String QM_TEST_PLAN = OSLC_QM_V2 + "testPlan";
	String QM_TEST_CASE = OSLC_QM_V2 + "testCase";
	String QM_TEST_SCRIPT = OSLC_QM_V2 + "testScript";
	String QM_TEST_RESULT = OSLC_QM_V2 + "testResult";
	String QM_TEST_EXECUTION_RECORD = OSLC_QM_V2 + "testExecutionRecord";

	/**
	 * @deprecated This is not part of the OSLC-QM 2.0 vocabulary.
	 */
	@Deprecated String QM_TEST_PLAN_QUERY = OSLC_QM_V2 + "TestPlanQuery";

	/**
	 * @deprecated This is not part of the OSLC-QM 2.0 vocabulary.
	 */
	@Deprecated String QM_TEST_CASE_QUERY = OSLC_QM_V2 + "TestCaseQuery";

	/**
	 * @deprecated This is not part of the OSLC-QM 2.0 vocabulary.
	 */
	@Deprecated String QM_TEST_SCRIPT_QUERY = OSLC_QM_V2 + "TestScriptQuery";

	/**
	 * @deprecated This is not part of the OSLC-QM 2.0 vocabulary.
	 */
	@Deprecated String QM_TEST_RESULT_QUERY = OSLC_QM_V2 + "TestResultQuery";

	/**
	 * @deprecated This is not part of the OSLC-QM 2.0 vocabulary.
	 */
	@Deprecated String QM_TEST_EXECUTION_RECORD_QUERY = OSLC_QM_V2 + "TestExecutionRecordQuery";

	//OSLC RM 2.0

	String RM_REQUIREMENT_TYPE = OSLC_RM_V2 + "Requirement";
	String RM_REQUIREMENT_COLLECTION_TYPE = OSLC_RM_V2 + "RequirementCollection";

	//OSLC AM 2.0
	String AM_RESOURCE_TYPE = OSLC_AM_V2 + "Resource";
	String AM_LINK_TYPE_TYPE = OSLC_AM_V2 + "LinkType";

	// RDF
	String RDF_TYPE_PROP		= RDF + "type";
	String RDFS_MEMBER 			= RDFS + "member";

	// DCTERMS URIs
	String DC_TITLE_PROP 		= DC + "title";
	String DC_DESC_PROP 		= DC + "description";
	String DC_TYPE_PROP 		= DC + "type";
	String DC_PUBLISHER_PROP 	= DC + "publisher";
	String DC_ID_PROP 			= DC + "identifier";
	String DC_NAME_PROP 		= DC + "name";
	String DC_CREATED_PROP		= DC + "created";
	String DC_MODIFIED_PROP		= DC + "modified";

	// DCTERMSs
	String DCTERMS_TITLE 		= DCTERMS + "title";
	String DCTERMS_DESC 		= DCTERMS + "description";
	String DCTERMS_TYPE 		= DCTERMS + "type";
	String DCTERMS_PUBLISHER 	= DCTERMS + "publisher";
	String DCTERMS_ID 			= DCTERMS + "identifier";
	String DCTERMS_NAME 		= DCTERMS + "name";
	String DCTERMS_CREATED		= DCTERMS + "created";
	String DCTERMS_MODIFIED		= DCTERMS + "modified";

    String OSLC2_0 = "2.0";
    String OSLC2_1 = "2.1";
    String OSLC3_0 = "3.0";
}
