/*******************************************************************************
 * Copyright (c) 2013 IBM Corporation.
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
 *	  Kevin Bauer - Initial implementation
 *******************************************************************************/
package org.eclipse.lyo.core.utils.marshallers;

public class OSLC4JContext {
	protected OSLC4JContext(){}
	
	public static OSLC4JContext newInstance(){
		return new OSLC4JContext();
	}
	
	public OSLC4JMarshaller createMarshaller(){
		return new OSLC4JMarshaller();
	}
	
	public OSLC4JUnmarshaller createUnmarshaller(){
		return new OSLC4JUnmarshaller();
	}
}
