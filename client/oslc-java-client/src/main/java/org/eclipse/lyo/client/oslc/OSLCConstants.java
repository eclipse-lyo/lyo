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
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Simple
 */

package org.eclipse.lyo.client.oslc;

/**
 * General OSLC constants
 *
 */
@Deprecated
public interface OSLCConstants {
	static String RFC_DATE_FORMAT = "yyyy-MM-dd'T'h:m:ss.S'Z'";

	static String DC = "http://purl.org/dc/terms/";
	static String DCTERMS ="dcterms:";
	static String RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
	static String RDFS = "http://www.w3.org/2000/01/rdf-schema#";
	static String ATOM = "http://www.w3.org/2005/Atom";
	static String OSLC_V2    = "http://open-services.net/ns/core#";
	static String CORE_DEFAULT = "http://open-services.net/ns/core#default";
	static String OSLC_CM_V2 = "http://open-services.net/ns/cm#";
	static String OSLC_AM_V2 = "http://open-services.net/ns/am#";
	static String OSLC_ASSET_V2 = "http://open-services.net/ns/asset#";
	static String OSLC_QM_V2 = "http://open-services.net/ns/qm#";
	static String OSLC_RM_V2 = "http://open-services.net/ns/rm#";
	static String OSLC_AUTO = "http://open-services.net/ns/auto#";

	// Version 1.0 namespace definitions
	static String OSLC_DISC = "http://open-services.net/xmlns/discovery/1.0/";
	static String OSLC_CM   = "http://open-services.net/xmlns/cm/1.0/";
	static String OSLC_QM   = "http://open-services.net/xmlns/qm/1.0/";
	static String OSLC_RM   = "http://open-services.net/xmlns/rm/1.0/";


	//--------------------------------------------------------------------------
	// Content-types for Accept header requests
	// Standard headers:
	static String CT_XML = "application/xml";
	static String CT_RDF = "application/rdf+xml";
	static String CT_JSON = "application/json";
	static String CT_ATOM = "application/atom+xml";

	// Version 1 headers:
	static String CT_CR_XML = "application/x-oslc-cm-change-request+xml";
	static String CT_CR_JSON = "application/x-oslc-cm-change-request+json";
	static String CT_CR_QUERY = "application/x-oslc-cm-change-request+xml";
	static String CT_DISC_CAT_XML = "application/x-oslc-disc-service-provider-catalog+xml";
	static String CT_DISC_DESC_XML = "application/x-oslc-cm-service-description+xml";

	// Version 2 headers:
	static String OSLC_CORE_VERSION = "OSLC-Core-Version";
	static String ETAG = "Etag";

	static String POST = "POST";
	static String SSL = "SSL";

	public static final String JENA_RDF_XML = "RDF/XML";

	//--------------------------------------------------------------------------
	// Property URIs

	// OSLC Core
	public static final String SERVICE_PROVIDER_PROP = OSLC_V2 + "serviceProvider";
	public static final String SERVICE_PROVIDER_TYPE = OSLC_V2 + "ServiceProvider";
	public static final String SERVICE_PROVIDER_CATALOG_PROP = OSLC_V2 + "serviceProviderCatalog";
	public static final String SERVICE_PROVIDER_CATALOG_TYPE = OSLC_V2 + "ServiceProviderCatalog";
	public static final String CREATION_PROP 		= OSLC_V2 + "creation";
	public static final String QUERY_CAPABILITY_PROP = OSLC_V2 + "QueryCapability";
	public static final String QUERY_BASE_PROP 		= OSLC_V2 + "queryBase";
	public static final String RESP_INFO_TYPE 		= OSLC_V2 + "ResponseInfo";
	public static final String SERVICE_PROP 		= OSLC_V2 + "service";
	public static final String DISCUSSION_PROP 		= OSLC_V2 + "discussion";
	public static final String INST_SHAPE_PROP 		= OSLC_V2 + "instanceShape";
	public static final String USAGE_PROP		 	= OSLC_V2 + "usage";
	public static final String USAGE_DEFAULT_URI 	= OSLC_V2 + "default";
	public static final String TOTAL_COUNT_PROP	 	= OSLC_V2 + "totalCount";
	public static final String RESOURCE_TYPE_PROP   = OSLC_V2 + "resourceType";
	public static final String RESOURCE_SHAPE_PROP  = OSLC_V2 + "resourceShape";
	public static final String DESCRIPTION_PROP 	= OSLC_V2 + "Description";
	// OSLC CM 2.0
	public static final String CM_CHANGE_REQUEST_TYPE = OSLC_CM_V2 + "ChangeRequest";
	public static final String CM_CLOSE_DATE_PROP 	= OSLC_CM_V2 + "closeDate";
	public static final String CM_STATUS_PROP 		= OSLC_CM_V2 + "status";
	public static final String CM_CLOSED_PROP 		= OSLC_CM_V2 + "closed";
	public static final String CM_INPROGRESS_PROP 	= OSLC_CM_V2 + "inprogress";
	public static final String CM_FIXED_PROP 		= OSLC_CM_V2 + "fixed";
	public static final String CM_APPROVED_PROP 	= OSLC_CM_V2 + "approved";
	public static final String CM_REVIEWED_PROP 	= OSLC_CM_V2 + "reviewed";
	public static final String CM_VERIFIED_PROP 	= OSLC_CM_V2 + "verified";

	// OSLC QM 2.0
	public static final String QM_TEST_PLAN = OSLC_QM_V2 + "testPlan";
	public static final String QM_TEST_CASE = OSLC_QM_V2 + "testCase";
	public static final String QM_TEST_SCRIPT = OSLC_QM_V2 + "testScript";
	public static final String QM_TEST_RESULT = OSLC_QM_V2 + "testResult";
	public static final String QM_TEST_EXECUTION_RECORD = OSLC_QM_V2 + "testExecutionRecord";

	/**
	 * @deprecated This is not part of the OSLC-QM 2.0 vocabulary.
	 */
	@Deprecated
	public static final String QM_TEST_PLAN_QUERY = OSLC_QM_V2 + "TestPlanQuery";

	/**
	 * @deprecated This is not part of the OSLC-QM 2.0 vocabulary.
	 */
	@Deprecated
	public static final String QM_TEST_CASE_QUERY = OSLC_QM_V2 + "TestCaseQuery";

	/**
	 * @deprecated This is not part of the OSLC-QM 2.0 vocabulary.
	 */
	@Deprecated
	public static final String QM_TEST_SCRIPT_QUERY = OSLC_QM_V2 + "TestScriptQuery";

	/**
	 * @deprecated This is not part of the OSLC-QM 2.0 vocabulary.
	 */
	@Deprecated
	public static final String QM_TEST_RESULT_QUERY = OSLC_QM_V2 + "TestResultQuery";

	/**
	 * @deprecated This is not part of the OSLC-QM 2.0 vocabulary.
	 */
	@Deprecated
	public static final String QM_TEST_EXECUTION_RECORD_QUERY = OSLC_QM_V2 + "TestExecutionRecordQuery";

	//OSLC RM 2.0

	public static final String RM_REQUIREMENT_TYPE = OSLC_RM_V2 + "Requirement";
	public static final String RM_REQUIREMENT_COLLECTION_TYPE = OSLC_RM_V2 + "RequirementCollection";

	//OSLC AM 2.0
	public static final String AM_RESOURCE_TYPE = OSLC_AM_V2 + "Resource";
	public static final String AM_LINK_TYPE_TYPE = OSLC_AM_V2 + "LinkType";

	// RDF
	public static final String RDF_TYPE_PROP		= RDF + "type";
	public static final String RDFS_MEMBER 			= RDFS + "member";

	// DCTERMS URIs
	public static final String DC_TITLE_PROP 		= DC + "title";
	public static final String DC_DESC_PROP 		= DC + "description";
	public static final String DC_TYPE_PROP 		= DC + "type";
	public static final String DC_PUBLISHER_PROP 	= DC + "publisher";
	public static final String DC_ID_PROP 			= DC + "identifier";
	public static final String DC_NAME_PROP 		= DC + "name";
	public static final String DC_CREATED_PROP		= DC + "created";
	public static final String DC_MODIFIED_PROP		= DC + "modified";

	// DCTERMSs
	public static final String DCTERMS_TITLE 		= DCTERMS + "title";
	public static final String DCTERMS_DESC 		= DCTERMS + "description";
	public static final String DCTERMS_TYPE 		= DCTERMS + "type";
	public static final String DCTERMS_PUBLISHER 	= DCTERMS + "publisher";
	public static final String DCTERMS_ID 			= DCTERMS + "identifier";
	public static final String DCTERMS_NAME 		= DCTERMS + "name";
	public static final String DCTERMS_CREATED		= DCTERMS + "created";
	public static final String DCTERMS_MODIFIED		= DCTERMS + "modified";

}
