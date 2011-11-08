/*******************************************************************************
 * Copyright (c) 2011 IBM Corporation.
 *
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the Eclipse Public License v1.0
 *  and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *  
 *  The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 *  and the Eclipse Distribution License is available at
 *  http://www.eclipse.org/org/documents/edl-v10.php.
 *  
 *  Contributors:
 *  
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

package org.eclipse.lyo.samples.bugzilla;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;

/**
 * Centralized URL creation
 */
public class URLStrategy {
    private static String baseUri = BugzillaInitializer.getBaseUri();
    private static String bugzillaUri = BugzillaInitializer.getBugzillaUri();
    
    public static String getChangeRequestURL(int id) {
        return baseUri + "/changerequest?id=" + id;
    }
    
    /** One URL for both creation, retrieval and query */
    public static String getChangeRequestCollectionURL(int productId) {
        return baseUri + "/changerequests?productId=" + productId;
    }

    public static String getServiceProviderCatalogURL() {
        return baseUri + "/catalog";
    }

    public static String getServiceProviderURL(int productId) {
        return baseUri + "/provider?productId=" + productId;
    }

    public static String getDelegatedCreationURL(int productId) {
        return baseUri + "/creator?productId=" + productId;
    }
    
    public static String getDelegatedSelectionURL(int productId) {
        return baseUri + "/selector?productId=" + productId;
    }
    
    public static String getCreationShapeURL(int productId) {
        return baseUri + "/creationshape?productId=" + productId;
    }
    
    public static String getQueryShapeURL(int productId) {
        return baseUri + "/queryshape?productId=" + productId;
    }
    
    public static String getQueryChangeRequestShapeURL(int productId) {
        return baseUri + "/queryshape?productId=" + productId + "&shape=changerequest";
    }

	public static String getPersonURL(String email)
			throws UnsupportedEncodingException {
		return baseUri + "/person?mbox=" + URLEncoder.encode(email, "UTF-8");
	}
    
    public static String getBugzillaBugURL(int id) {
        return BugzillaInitializer.getBugzillaUri() + "/show_bug.cgi?id=" + id;
    }

    public static String getIconURL() {
        return bugzillaUri + "/images/favicon.ico";
    }
    
    /**
     * Defines what label to use for Bug URLs in: delegated dialog responses and UI preview
     * @param bugId
     * @param summary
     * @return A formated one-liner combining bug id and summary
     */
    public static String getChangeRequestLinkLabel(int bugId, String summary) {
        return "Bug " + bugId + ": " + summary;
    }
    
    
}

