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
 *     Michael Fiedler       - initial API and implementation
 *     
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.core;

import org.eclipse.lyo.oslc4j.core.OSLC4JConstants;

public class OSLC4JUtils {
	
	/**
	 * Returns the value of org.eclipse.lyo.oslc4j.core.public.uri or null if not set.
	 * 
	 * 
	 * @return
	 */
	public static String getPublicURI()
	{
		String publicURI = System.getProperty(OSLC4JConstants.OSLC4J_PUBLIC_URI);
		return publicURI;
	}
	
	/**
	 * Sets the value of org.eclipse.lyo.oslc4j.core.public.uri
	 * @param publicURI
	 */
	public static void setPublicURI(String publicURI)
	{
		if (publicURI != null && !publicURI.isEmpty())
		{
			System.setProperty(OSLC4JConstants.OSLC4J_PUBLIC_URI, publicURI);
		}
	}

}
