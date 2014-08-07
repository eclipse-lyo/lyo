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
 *	   Russell Boykin		- initial API and implementation
 *	   Alberto Giammaria	- initial API and implementation
 *	   Chris Peters			- initial API and implementation
 *	   Gianluca Bernardini	- initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.test.resources;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;

import org.eclipse.lyo.oslc4j.core.annotation.OslcDialog;
import org.eclipse.lyo.oslc4j.core.annotation.OslcQueryCapability;
import org.eclipse.lyo.oslc4j.core.annotation.OslcService;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;
import org.eclipse.lyo.oslc4j.test.Constants;
import org.eclipse.lyo.oslc4j.test.Persistence;
import org.eclipse.lyo.oslc4j.test.Test;

@OslcService(Constants.TEST_DOMAIN)
@Path("tests2")
public class TestCollectionResource
{
	public TestCollectionResource()
	{
		super();
	}

	@OslcDialog
	(
		title = "Test Collection Selection Dialog",
		label = "Test Collection Selection Dialog",
		uri = "",
		hintWidth = "1000px",
		hintHeight = "600px",
		resourceTypes = {Constants.TYPE_TEST},
		usages = {Constants.USAGE_COLLECTION}
	)
	@OslcQueryCapability
	(
		title = "Test Collection Query Capability",
		label = "Test Collection Query",
		resourceShape = OslcConstants.PATH_RESOURCE_SHAPES + "/" + Constants.PATH_TEST,
		resourceTypes = {Constants.TYPE_TEST},
		usages = {Constants.USAGE_COLLECTION}
	)
	@GET
	@Produces({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_XML, OslcMediaType.TEXT_XML, OslcMediaType.APPLICATION_JSON, OslcMediaType.TEXT_TURTLE})
	public List<Test> getTests()
	{
		return new ArrayList<Test>(Arrays.asList(Persistence.getTests()));
	}
}
