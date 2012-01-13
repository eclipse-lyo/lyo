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

import java.io.IOException;
import java.util.Properties;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.eclipse.lyo.samples.bugzilla.exception.UnauthroziedException;
import org.eclipse.lyo.samples.bugzilla.utils.HttpUtils;

import com.j2bugzilla.base.BugzillaConnector;
import com.j2bugzilla.base.BugzillaException;
import com.j2bugzilla.base.ConnectionException;
import com.j2bugzilla.rpc.LogIn;

public class BugzillaInitializer {

    private static final String CONNECTOR_SESSION_ATTRIBUTE = "org.eclipse.lyo.samples.bugzilla.BugzillaConnector";
	private static String baseUri = null;
    private static String bugzillaUri = null;
    private static boolean provideHtml = true;
    
    static {
        Properties props = new Properties();
        try {
            props.load(BugzillaInitializer.class.getResourceAsStream("/bugz.properties"));
            baseUri     = props.getProperty("adapter_uri");
            bugzillaUri = props.getProperty("bugzilla_uri");
            if (props.getProperty("provideHtml") != null) {
                provideHtml = Boolean.parseBoolean(props.getProperty("provideHtml"));
            }
            System.out.println("adapter_uri: "  + baseUri);
            System.out.println("bugzilla_uri: " + bugzillaUri);
            System.out.println("provideHtml: "  + provideHtml);
            
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

	public static BugzillaConnector getBugzillaConnector(Credentials credentials)
			throws ConnectionException, UnauthroziedException {
		BugzillaConnector bc = new BugzillaConnector();
		bc.connectTo(bugzillaUri + "/xmlrpc.cgi");
		LogIn login = new LogIn(credentials.getUsername(), credentials.getPassword());
		try {
			bc.executeMethod(login);
		} catch (BugzillaException e) {
			throw new UnauthroziedException(e.getCause().getMessage());
		}
		return bc;
	}

	public static BugzillaConnector getBugzillaConnector(
			HttpServletRequest request) throws ConnectionException,
			UnauthroziedException {
		HttpSession session = request.getSession();
		BugzillaConnector connector = (BugzillaConnector) session
				.getAttribute(CONNECTOR_SESSION_ATTRIBUTE);
		if (connector == null) {
			Credentials credentials = HttpUtils.getCredentials(request);
			if (credentials == null) {
				throw new UnauthroziedException();
			}
			connector = getBugzillaConnector(credentials);
			session.setAttribute(CONNECTOR_SESSION_ATTRIBUTE, connector);
		}
		
		return connector;
	}

	public static String getBaseUri() {
        return baseUri;
    }

    public static void setBaseUri(String baseUri) {
        BugzillaInitializer.baseUri = baseUri;
    }

    public static String getBugzillaUri() {
        return bugzillaUri;
    }

    public static void setBugzillaUri(String bugzillaUri) {
        BugzillaInitializer.bugzillaUri = bugzillaUri;
    }

    public static boolean isProvideHtml() {
        return provideHtml;
    }

    public static void setProvideHtml(boolean aProvideHtml) {
        provideHtml = aProvideHtml;
    }
}
