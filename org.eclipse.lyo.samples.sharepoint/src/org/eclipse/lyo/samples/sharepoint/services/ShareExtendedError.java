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
package org.eclipse.lyo.samples.sharepoint.services;

public class ShareExtendedError {
	
	private int height;
	private int width;
	private String url;
	private String rel;
	
	public ShareExtendedError( String rel, String url, int height, int width ){
		this.rel = rel;
		this.url = url;
		this.height = height;
		this.width = width;
	}
	
	public String toString(){
		StringBuilder sb = new StringBuilder();
		sb.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");//$NON-NLS-1$ 
		sb.append("<rdf:RDF \n"); //$NON-NLS-1$ 
		sb.append("\txmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\n"); //$NON-NLS-1$ 
		sb.append("\txmlns:oslc=\"http://open-services.net/ns/core#\">\n"); //$NON-NLS-1$ 
		sb.append("<oslc:ExtendedError>\n"); //$NON-NLS-1$ 
		sb.append("\t\t<oslc:rel>" + rel + "</oslc:rel>\n"); //$NON-NLS-1$ //$NON-NLS-2$
		sb.append("\t\t<oslc:moreInfp rdf:resource=\"" + url + "\" />" ); //$NON-NLS-1$ //$NON-NLS-2$
		sb.append("\t\t<oslc:hintHeight>" + height + "</oslc:hintHeight>\n"); //$NON-NLS-1$ //$NON-NLS-2$
		sb.append("\t\t<oslc:hintWidth>" + width + "</oslc:hintWidth>\n"); //$NON-NLS-1$ //$NON-NLS-2$
		sb.append("\t</oslc:ExtendedError>\n"); //$NON-NLS-1$
		return sb.toString();
	}
	
	public int getHeight() {
		return height;
	}
	public void setHeight(int height) {
		this.height = height;
	}
	public int getWidth() {
		return width;
	}
	public void setWidth(int width) {
		this.width = width;
	}
	public String getUrl() {
		return url;
	}
	public void setUrl(String url) {
		this.url = url;
	}
	public String getRel() {
		return rel;
	}
	public void setRel(String rel) {
		this.rel = rel;
	}

}
