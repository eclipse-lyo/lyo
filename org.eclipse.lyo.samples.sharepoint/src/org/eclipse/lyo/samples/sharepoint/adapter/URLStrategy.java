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

package org.eclipse.lyo.samples.sharepoint.adapter;

/**
 * Centralized URL creation
 */
public class URLStrategy {
    private static String baseUri = SharepointInitializer.getBaseUri();
    private static String sharepointUri = SharepointInitializer.getSharepointUri();
    
    public static String getChangeRequestURL(int id) {
        return baseUri + "/changerequest?id=" + id;
    }
    
    /** One URL for both creation, retrieval and query */
    public static String getChangeRequestCollectionURL(String collection) {
        return baseUri + "/changerequests?collection=" + collection;
    }

    public static String getServiceProviderCatalogURL() {
        return baseUri + "/catalog";
    }
    
    public static String getResourceBaseURL() {
        return baseUri + "/resource";
    }

    public static String getServiceProviderURL(String collection) {
        return baseUri + "/provider?collection=" + collection;
    }

    public static String getDelegatedCreationURL(String collection) {
        return baseUri + "/creator?collection=" + collection;
    }
    
    public static String getDelegatedSelectionURL(String collection) {
        return baseUri + "/selector?collection=" + collection;
    }
    
    public static String getCreationShapeURL(String collection) {
        return baseUri + "/creationshape?collection=" + collection;
    }
    
    public static String getQueryShapeURL(String collection) {
        return baseUri + "/queryshape?collection=" + collection;
    }
    
    public static String getQueryChangeRequestShapeURL(String collection) {
        return baseUri + "/queryshape?collection=" + collection + "&shape=changerequest";
    }
    
    public static String getSharepointDocURL(int id) {
        return SharepointInitializer.getSharepointUri() + "/show_bug.cgi?id=" + id;
    }

//    public static String getIconURL() {
//        return sharepointUri + "/images/favicon.ico";
//    }
    
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

