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
package org.eclipse.lyo.core.trs;

import java.net.URI;

import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;

import static org.eclipse.lyo.core.trs.TRSConstants.*;

@OslcNamespace(TRS_NAMESPACE)
@OslcResourceShape(title = "Modification  Shape", describes = TRS_TYPE_MODIFICATION)
public class Modification extends ChangeEvent {
	
	public Modification() {}
	
	/**
	 * @param changed
	 * @param order
	 */
	public Modification(URI about, URI changed, int order) {
		super(about, changed, order);
	}
}
