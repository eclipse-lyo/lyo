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
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.samples.bugzilla.test;

import java.util.Vector;

import org.eclipse.lyo.samples.bugzilla.utils.AcceptType;


import junit.framework.TestCase;


public class TestAcceptType extends TestCase {
	
	public void testAcceptStrings() {		

		Vector<String> accept1 = new Vector<String>();
		accept1.add("text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8");		
		assertTrue( AcceptType.willAccept("text/html", accept1.elements()));
				
		Vector<String> accept2 = new Vector<String>();
		accept2.add("application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5");		
		assertTrue( AcceptType.willAccept("text/html", accept2.elements()));

	}
}
