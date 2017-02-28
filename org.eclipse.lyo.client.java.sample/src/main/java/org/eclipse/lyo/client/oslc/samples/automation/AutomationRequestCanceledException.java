/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
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
 *     Paul McMahan <pmcmahan@us.ibm.com>     - initial implementation
 *******************************************************************************/
package org.eclipse.lyo.client.oslc.samples.automation;

import org.eclipse.lyo.client.oslc.resources.AutomationRequest;

/**
 * An exception thrown when an AutomationRequest has been canceled
 */
public class AutomationRequestCanceledException extends AutomationException {

	private final AutomationRequest request;

	private static final long serialVersionUID = 1L;

	public AutomationRequestCanceledException(AutomationRequest request) {
		super();
		this.request = request;
	}

	public AutomationRequest getCanceledRequest() {
		return request;
	}

}
