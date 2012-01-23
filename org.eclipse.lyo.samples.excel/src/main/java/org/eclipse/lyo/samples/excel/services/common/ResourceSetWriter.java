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

import java.io.IOException;
import java.io.OutputStream;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.StreamingOutput;

import org.eclipse.lyo.samples.excel.adapter.common.ResourceSet;


public class ResourceSetWriter implements StreamingOutput {
	ResourceSet resultSet;
	public ResourceSetWriter(ResourceSet resultSet) {
		this.resultSet = resultSet;
	}
	@Override
	public void write(OutputStream output) throws IOException, WebApplicationException {
		resultSet.outputAsXML(output);
	}
}
