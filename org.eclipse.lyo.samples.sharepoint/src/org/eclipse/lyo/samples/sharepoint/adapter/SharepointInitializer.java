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

import java.io.IOException;
import java.util.Properties;

import org.eclipse.lyo.samples.sharepoint.SharepointConnector;
import org.eclipse.lyo.samples.sharepoint.exceptions.ConnectionException;
import org.eclipse.lyo.samples.sharepoint.exceptions.SharepointException;


public class SharepointInitializer {

    private static String baseUri = null;
    private static String sharepointUri = null;
    private static String username = null;
    private static String password = null;
    private static boolean provideHtml = true;

    static {
        Properties props = new Properties();
        try {
            props.load(SharepointInitializer.class.getResourceAsStream("/sharepoint.properties"));
            baseUri     = props.getProperty("adapter_uri");
            sharepointUri = props.getProperty("sharepoint_uri");
            username    = props.getProperty("username");
            password    = props.getProperty("password");
            if (props.getProperty("provideHtml") != null) {
                provideHtml = Boolean.parseBoolean(props.getProperty("provideHtml"));
            }
            //System.out.println("adapter_uri: "  + baseUri);
            //System.out.println("sharepoint_uri: " + sharepointUri);
            //System.out.println("username: "     + username);
            //System.out.println("password: "     + password);
            //System.out.println("provideHtml: "  + provideHtml);
            
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static SharepointConnector getSharepointConnector() throws ConnectionException, SharepointException {
        SharepointConnector bc = new SharepointConnector();
        bc.connectTo(sharepointUri);
        return bc;
    }

    public static String getBaseUri() {
        return baseUri;
    }

    public static void setBaseUri(String baseUri) {
        SharepointInitializer.baseUri = baseUri;
    }

    public static String getSharepointUri() {
        return sharepointUri;
    }
    
    public static String getSharepointNormalUri() {
        return sharepointUri.substring(0, sharepointUri.indexOf("/_vti_bin/listdata.svc") );
    }

    public static void setSharepointUri(String sharepointUri) {
        SharepointInitializer.sharepointUri = sharepointUri;
    }

    public static String getUsername() {
        return username;
    }

    public static void setUsername(String username) {
        SharepointInitializer.username = username;
    }

    public static String getPassword() {
        return password;
    }

    public static void setPassword(String password) {
        SharepointInitializer.password = password;
    }

    public static boolean isProvideHtml() {
        return provideHtml;
    }

    public static void setProvideHtml(boolean aProvideHtml) {
        provideHtml = aProvideHtml;
    }
}
