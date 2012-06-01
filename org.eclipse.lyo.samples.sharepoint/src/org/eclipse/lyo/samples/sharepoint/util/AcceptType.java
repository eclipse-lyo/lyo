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
package org.eclipse.lyo.samples.sharepoint.util;

import java.util.Enumeration;

import javax.servlet.http.HttpServletRequest;

public class AcceptType implements Comparable<AcceptType> {
	public String type;
	public int weight;
	public AcceptType(String typeExp) {
		int pos = typeExp.indexOf(';'); 
		if( pos > 0 ) {
			this.type = typeExp.substring(0,pos).trim();
			String qualifier = typeExp.substring(pos+1);
			if( qualifier.startsWith("q=") ) {
				try{
					float w = Float.parseFloat(qualifier.substring(2)) * 1000;
					this.weight = (int) w;
				}catch( NumberFormatException e ) {
					this.weight = 1000;
				}
			} else {
				this.weight = 1000;
			}
			
		} else {
			this.type = typeExp;
			this.weight = new Integer(1000);
		}
	}
	
	public static boolean willAccept(String contentType, Enumeration acceptHds) {
		while(acceptHds.hasMoreElements()) {
			String acceptHdr = (String) acceptHds.nextElement();
			// split up the individual content types
			String[] hdrs = acceptHdr.split(",");
			for (String acctHdrVal : hdrs) {
				AcceptType acceptType = new AcceptType( acctHdrVal);
				if( acceptType.equals(contentType) ) return true;
			}
		}
		return false;
	}
	
	public static boolean willAccept(String contentType, HttpServletRequest req) {
		@SuppressWarnings("rawtypes")
		Enumeration acceptHds = req.getHeaders("Accept");
		while(acceptHds.hasMoreElements()) {
			String acceptHdr = (String) acceptHds.nextElement();
			// split up the individual content types
			String[] qVals = acceptHdr.split(";");
			String[] hdrs = qVals[0].split(",");
			for (String acctHdrVal : hdrs) {
				AcceptType acceptType = new AcceptType( acctHdrVal);
				if( acceptType.equals(contentType) ) return true;
			}
		}
		return false;
	}
	
	@Override
	public int compareTo(AcceptType other) {
		return ((AcceptType)other).weight - weight;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (this.type.length() == 0) return false;
		if( obj instanceof AcceptType ) {
			AcceptType otherAcceptType = (AcceptType) obj;
			// now we need to sort out wildcards
			String[] otc = otherAcceptType.type.split("/");
			String[] ttc = this.type.split("/");
			boolean seg1 = compareSegment(otc[0], ttc[0]);
			boolean seg2 = compareSegment(otc[1], ttc[1]);
			return seg1 && seg2;
		} else if (obj != null && obj instanceof String ) {
			String str = (String)obj;
			if (str.length() == 0) return false;
			String[] otc = str.split("/");
			String[] ttc = this.type.split("/");
			boolean seg1 = compareSegment(otc[0], ttc[0]);
			boolean seg2 = compareSegment(otc[1], ttc[1]);
			return seg1 && seg2;
		}
		return false;
	}
	
	public boolean compareSegment(String s1, String s2 ) {
		if( s1.equals("*") || s2.equals("*") ) {
			return true; 
		} 
		return s1.equals(s2);
	}
}