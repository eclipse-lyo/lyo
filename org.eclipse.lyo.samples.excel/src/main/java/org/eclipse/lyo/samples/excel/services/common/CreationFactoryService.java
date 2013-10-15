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
 *     Masaki Wakao 
 *     Yoshio Horiuchi 
 *     Kohji Ohsawa 
 *******************************************************************************/
package org.eclipse.lyo.samples.excel.services.common;

import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriInfo;

import org.apache.commons.lang.NotImplementedException;
import org.eclipse.lyo.rio.core.IConstants;

import com.hp.hpl.jena.rdf.model.Model;

@Path(IConstants.SERVICE_SERVICES + "/{projectId}/factory/changerequest")
public class CreationFactoryService {

	@POST
	//@Consumes ({"application/rdf+xml", "application/xml"})
	public Response doPost(@PathParam("projectId") String projectId, @Context UriInfo uriInfo, Model changeRequest) {
		throw new NotImplementedException();
	}
}
