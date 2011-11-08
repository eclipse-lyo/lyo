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

import junit.framework.TestCase;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;


public class TestParser extends TestCase {
	
	public void testParsing() {		
		
    	Model model = ModelFactory.createDefaultModel();		
    	model.read(this.getClass().getResourceAsStream("/bug2.xml"), ""); 
    	Resource resource = model.getResource(""); 		
		
    	Property titleProp = model.getProperty("http://purl.org/dc/terms/title");
    	Statement title = model.getProperty(resource, titleProp);
    	System.err.println("Title = " + title.getString());

    	Property productProp = model.getProperty("http://www.bugzilla.org/rdf#product");
    	Statement product = model.getProperty(resource, productProp);
    	System.err.println("Product = " + product.getString());

    	Property componentProp = model.getProperty("http://www.bugzilla.org/rdf#component");
    	Statement component = model.getProperty(resource, componentProp);
    	System.err.println("Component = " + component.getString());

	}
}
