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
 *    Michael Fiedler - initial API and implementation
 *******************************************************************************/

package org.eclipse.lyo.client;

/**
 * Constants for Jazz rootservices entries
 *
 */
public interface RootServicesConstants {

	//rootservices catalog properties.  See starting at:
	//https://jazz.net/wiki/bin/view/Main/RootServicesSpec#Change_Management_Service_Provid

	static String CM_ROOTSERVICES_CATALOG_PROP = "cmServiceProviders";
	static String QM_ROOTSERVICES_CATALOG_PROP = "qmServiceProviders";
	static String RM_ROOTSERVICES_CATALOG_PROP = "rmServiceProviders";
	static String AM_ROOTSERVICES_CATALOG_PROP = "amServiceProviders";
	static String AUTO_ROOTSERVICES_CATALOG_PROP = "autoServiceProviders";

	//OAuth entries
	static String OAUTH_REQUEST_TOKEN_URL = "oauthRequestTokenUrl";
	static String OAUTH_USER_AUTH_URL     = "oauthUserAuthorizationUrl";
	static String OAUTH_ACCESS_TOKEN_URL  = "oauthAccessTokenUrl";
	static String OAUTH_REALM_NAME        = "oauthRealmName";

	//https://jazz.net/wiki/bin/view/Main/RootServicesSpecAddendum2
	static String OAUTH_REQUEST_CONSUMER_KEY_URL = "oauthRequestConsumerKeyUrl";
	static String OAUTH_APPROVAL_MODULE_URL = "oauthApprovalModuleUrl";
}
