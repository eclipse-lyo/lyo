/*******************************************************************************
 * Copyright (c) 2012, 2013 IBM Corporation.
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
 *	  Steve Pitschke - initial API and implementation
 *	  Sameul Padgett - fix ClassCastException calling value()
 *******************************************************************************/

package org.eclipse.lyo.core.query;

/**
 * URI_REF operand from oslc.where clause
 */
public interface UriRefValue extends Value
{
	String value();
}
