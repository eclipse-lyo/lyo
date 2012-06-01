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
package org.eclipse.lyo.samples.sharepoint.store;


public class ShareStatement {

	private String subject;
	private boolean bnode = false;
	private String predicate;
	private ShareValue object;
	private String context;

	public ShareStatement(String subject, String predicate, ShareValue object, String context) {
		bnode = !( subject.startsWith("http://") || subject.startsWith("https://") );
			
		this.subject = subject;
		this.predicate = predicate;
		this.object = object;
		this.context = context;
	}

	public String getSubject() {
		return subject;
	}
	
	public boolean isBNode(){
		return bnode;
	}

	public void setSubject(String subject) {
		this.subject = subject;
	}

	public String getPredicate() {
		return predicate;
	}

	public void setPredicate(String predicate) {
		this.predicate = predicate;
	}

	public ShareValue getObject() {
		return object;
	}

	public void setObject(ShareValue object) {
		this.object = object;
	}

	public String getContext() {
		return context;
	}

	public void setContext(String context) {
		this.context = context;
	}

	@SuppressWarnings("nls")
	@Override
	public String toString() {
		if( this.subject.startsWith("http://") || this.subject.startsWith("https://") ) {
			return '<' + this.subject + "> <" + this.predicate + "> " + object.toString(); 
		} else {
			return this.subject + " <" + this.predicate + "> " + object.toString();
		}
	}

	
	
}
