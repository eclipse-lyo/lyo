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
package org.eclipse.lyo.client;

/**
 * Constants for Jazz rootservices entries
 *
 */
public interface RootServicesConstants {

    // rootservices catalog properties.  See starting at:
    // https://jazz.net/wiki/bin/view/Main/RootServicesSpec#Change_Management_Service_Provid

    static String CM_ROOTSERVICES_CATALOG_PROP = "cmServiceProviders";
    static String QM_ROOTSERVICES_CATALOG_PROP = "qmServiceProviders";
    static String RM_ROOTSERVICES_CATALOG_PROP = "rmServiceProviders";
    static String AM_ROOTSERVICES_CATALOG_PROP = "amServiceProviders";
    static String AUTO_ROOTSERVICES_CATALOG_PROP = "autoServiceProviders";

    // OAuth entries
    static String OAUTH_REQUEST_TOKEN_URL = "oauthRequestTokenUrl";
    static String OAUTH_USER_AUTH_URL = "oauthUserAuthorizationUrl";
    static String OAUTH_ACCESS_TOKEN_URL = "oauthAccessTokenUrl";
    static String OAUTH_REALM_NAME = "oauthRealmName";

    // https://jazz.net/wiki/bin/view/Main/RootServicesSpecAddendum2
    static String OAUTH_REQUEST_CONSUMER_KEY_URL = "oauthRequestConsumerKeyUrl";
    static String OAUTH_APPROVAL_MODULE_URL = "oauthApprovalModuleUrl";
}
