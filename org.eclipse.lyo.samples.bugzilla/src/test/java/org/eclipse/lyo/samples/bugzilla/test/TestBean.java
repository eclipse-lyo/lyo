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

import java.net.URI;
import java.net.URISyntaxException;

import org.eclipse.lyo.samples.bugzilla.resources.BugzillaChangeRequest;
import org.eclipse.lyo.samples.bugzilla.resources.Person;

import thewebsemantic.Bean2RDF;
import junit.framework.TestCase;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;

public class TestBean extends TestCase {
	public void testBugzillaChangeRequestBean() throws URISyntaxException {
		BugzillaChangeRequest cr = createChangeRequest();
		
		Model m = ModelFactory.createOntologyModel();
		Bean2RDF writer = new Bean2RDF(m);
		writer.save(cr);
		
		m.write(System.out);
	}

	private BugzillaChangeRequest createChangeRequest()
			throws URISyntaxException {
		BugzillaChangeRequest cr = new BugzillaChangeRequest();
		cr.setUri(new URI("http://localhost:8282/bugz/24"));
		cr.setTitle("New Bug: " + System.currentTimeMillis());
		
		Person nina = new Person();
		nina.setName("Nina Example");
		nina.setEmail("nina@example.com");
		
		cr.setContributor(nina);
		cr.setProduct("FakePortal");
		cr.setComponent("Datastore");
		cr.setVersion("1.0");
		cr.setOperatingSystem("Mac OS");
		cr.setPlatform("Macintosh");

		return cr;
	}
}
