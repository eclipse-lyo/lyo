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
package org.eclipse.lyo.samples.bugzilla.test;

import java.net.HttpURLConnection;
import java.net.URL;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;


public class SanityCheck { // extends TestCase {
    
    public void testParsing() throws Exception {
        parse("http://localhost:8282/bugz/catalog");
        parse("http://localhost:8282/bugz/provider?productId=2");        
        parse("http://localhost:8282/bugz/changerequests?productId=2");
        parse("http://localhost:8282/bugz/changerequest?id=6");        
        parse("http://localhost:8282/bugz/creationshape?productId=2");
        parse("http://localhost:8282/bugz/queryshape?productId=2");
    }
	
	public void parse(String urlstr) throws Exception {					
        URL url = new URL(urlstr);
        HttpURLConnection conn = (HttpURLConnection)url.openConnection();
        conn.setRequestProperty("Accept", "application/rdf+xml");
        Model model = ModelFactory.createDefaultModel();			
        model.read(conn.getInputStream(), urlstr);
        //Resource resource = model.getResource(urlstr);
        System.out.println("OK - " + urlstr);
	}
}
