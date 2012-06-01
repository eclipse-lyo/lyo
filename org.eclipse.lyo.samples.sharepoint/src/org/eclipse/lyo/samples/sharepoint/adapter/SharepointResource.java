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

import java.util.List;

import org.eclipse.lyo.samples.sharepoint.common.IAmConstants;
import org.eclipse.lyo.samples.sharepoint.core.IConstants;
import org.eclipse.lyo.samples.sharepoint.store.OslcResource;
import org.eclipse.lyo.samples.sharepoint.store.ShareResource;
import org.eclipse.lyo.samples.sharepoint.store.ShareServerException;
import org.eclipse.lyo.samples.sharepoint.store.ShareStatement;


public class SharepointResource extends OslcResource {

	public SharepointResource(String uri) throws ShareServerException {
		super(uri);
	}

	public SharepointResource(String uri, List<ShareStatement> statements) {
		super(uri, statements);
	}
	
	public SharepointResource(ShareResource shareResource) throws ShareServerException {
		super(shareResource.getUri());
		this.addStatements(shareResource.getStatements());
	}	
    
	public void setApprovalStatus(String status) throws ShareServerException {
		this.setStringProperty(IConstants.CM_STATUS_PROP , status);
	}

	public String getApprovalStatus() {
		String status = this.getFirstStringProperty(IConstants.CM_STATUS_PROP );
		switch (Integer.parseInt(status)) {
		   case 0: return "Approved";
		   case 1: return "Rejected";
		   case 2: return "Pending";
		   case 3: return "Draft";
		   case 4: return "Scheduled";
		   default: return status;
		}	
		
	}
	
	public void setTitle(String title) throws ShareServerException {
		this.setStringProperty(IConstants.DCTERMS_DESCRIPTION, title);
	}

	public String getTitle() {
		return this.getFirstStringProperty(IConstants.DCTERMS_DESCRIPTION);
	}

	public String getSource() {
		return this.getFirstUriProperty(IConstants.DCTERMS_SOURCE);
	}
	
	public void setSource(String source) throws ShareServerException {
		this.setStringProperty(IConstants.DCTERMS_SOURCE, source);
	}

	public String getSourceContentType() {
		return this.getFirstStringProperty(IAmConstants.RIO_AM_SOURCE_CONTENT_TYPE);
	}
	
	public void setSourceContentType(String contentType) throws ShareServerException {
		this.setStringProperty(IAmConstants.RIO_AM_SOURCE_CONTENT_TYPE, contentType);
	}
	
	public void setDescription(String description) throws ShareServerException {
		this.setStringProperty(IConstants.DCTERMS_DESCRIPTION, description);
	}

	public String getDescription() {
		return this.getFirstStringProperty(IConstants.DCTERMS_DESCRIPTION);
	}
	
}
