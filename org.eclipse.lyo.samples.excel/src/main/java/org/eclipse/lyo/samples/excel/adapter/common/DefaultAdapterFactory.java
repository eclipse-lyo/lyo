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
package org.eclipse.lyo.samples.excel.adapter.common;

public class DefaultAdapterFactory {
	private static String className = "org.eclipse.lyo.samples.excel.adapter.ExcelAdapterFactory";
	
	private static ResourceAdapterFactory adapterFactory = null;
	static {
		try {
			Class<ResourceAdapterFactory> clazz = (Class<ResourceAdapterFactory>) Class.forName(className);
			adapterFactory = clazz.newInstance();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public static void setAdapterFactory(ResourceAdapterFactory adapterFactory) { 
		DefaultAdapterFactory.adapterFactory = adapterFactory;
	}
	public static ResourceAdapter createAdapter() {
		if (adapterFactory != null) {
			return adapterFactory.createAdapter();
		}
		return null;
	}
}
