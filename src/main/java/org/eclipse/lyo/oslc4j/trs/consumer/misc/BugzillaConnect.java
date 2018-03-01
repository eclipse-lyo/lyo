/**
 * Copyright (c) 2016-2017   KTH Royal Institute of Technology.
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
 * Omar Kacimi         -  Initial implementation
 * Andrew Berezovskyi  -  Lyo contribution updates
 */
package org.eclipse.lyo.oslc4j.trs.consumer.misc;

import java.net.MalformedURLException;
import java.net.URL;
import java.text.ParseException;
import java.util.HashMap;
import java.util.Map;

import org.apache.xmlrpc.XmlRpcException;
import org.apache.xmlrpc.client.XmlRpcClient;
import org.apache.xmlrpc.client.XmlRpcClientConfigImpl;

/**
 * * connecting to bugzilla through java xml-rpc api and that could be used to
 * generate random bug data.
 *
 * @author Omar
 *
 */
public class BugzillaConnect {

    private static final String serverUrl = "http://10.238.2.145/xmlrpc.cgi";
    private static final String adminUser = "";
    private static final String adminPassword = "";
    private static final String assigneeUser = "omar.kacimi@gmail.com";

    public static void main(String[] args) throws MalformedURLException, XmlRpcException, ParseException {

        XmlRpcClient client = new XmlRpcClient();
        XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();

        // Setting timeouts for xmlrpc calls made using
        // XmlRpcSunHttpTransportFactory, the default connection factory int
        int xmlrpcConnTimeout = 120000; // Connection timeout
        int xmlrpcReplyTimeOut = 120000; // Reply timeout

        config.setServerURL(new URL(serverUrl));
        config.setConnectionTimeout(xmlrpcConnTimeout);
        config.setReplyTimeout(xmlrpcReplyTimeOut);
        client.setConfig(config);

        login(client, adminUser, adminPassword);

        createBug(client);
    }

    private static void login(XmlRpcClient client, String name, String password) {
        try {
            // map of the login data
            Map<String, String> loginMap = new HashMap<String, String>();
            loginMap.put("login", name);
            loginMap.put("password", password);

            // login to bugzilla
            Map<?, ?> loginResult = (Map<?, ?>) client.execute("User.login", new Object[] { loginMap });
            System.out.println(name + " logged in, id=" + loginResult.get("id"));
        } catch (XmlRpcException e) {
            System.out.println(name + ": error " + e.code);
        }
    }

    private static void createBug(XmlRpcClient client) {
        long time = System.nanoTime();
        try {
            // map of the bug data
            Map<String, String> bugMap = new HashMap<String, String>();

            bugMap.put("product", "TestProduct");
            bugMap.put("component", "TestComponent");
            bugMap.put("summary", "A bug with a specific assignee");
            bugMap.put("description", "This will be a comment");
            bugMap.put("version", "unspecified");
            bugMap.put("op_sys", "Linux");
            bugMap.put("platform", "PC");
            bugMap.put("assigned_to", assigneeUser);

            // create bug
            Object createResult = client.execute("Bug.create", new Object[] { bugMap });
            System.out.println("createResult = " + createResult);
        } catch (XmlRpcException e) {
            System.out.println("Creation error: " + e.code);
            System.out.println(e.getMessage());
        }
        System.out.println((System.nanoTime() - time) / 1000000);
    }
}
