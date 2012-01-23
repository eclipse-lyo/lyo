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
package org.eclipse.lyo.examples.excel.services.common;

import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStream;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.StreamingOutput;
import javax.ws.rs.core.UriInfo;

import org.eclipse.lyo.examples.excel.adapter.common.AdapterRegistry;
import org.eclipse.lyo.examples.excel.adapter.common.ResourceAdapter;
import org.eclipse.lyo.rio.core.IConstants;

@Path(IConstants.SERVICE_SERVICES + "/{projectId}/download.xls/")
public class DownloadService {

	@GET
	@Produces("application/octet-stream")
	public StreamingOutput getQueryResult(
			@Context UriInfo uriInfo, 
			@PathParam("projectId") String projectId
			){
		try{
			String baseUrl = uriInfo.getBaseUri().toString() + IConstants.SERVICE_SERVICES;
			ResourceAdapter adapter = AdapterRegistry.getAdapter(baseUrl);
			String fileName = adapter.getDefaultExcelAbsolutePath();
			
			File file = new File(fileName);
	        
	        final byte[] bbuf = new byte[1024];
	        final DataInputStream in = new DataInputStream(new FileInputStream(file));
			
			return new StreamingOutput() {
	            public void write(OutputStream output) throws IOException, WebApplicationException {
	            	int length = 0;
	            	while ((in != null) && ((length = in.read(bbuf)) != -1)){
	            		output.write(bbuf, 0, length);
	            	}
	            }
	        };
		}catch (FileNotFoundException e){
			e.printStackTrace();
		}
		return null;
	}

}
