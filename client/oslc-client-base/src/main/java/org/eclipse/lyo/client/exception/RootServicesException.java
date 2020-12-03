/*
 * Copyright (c) 2020 Contributors to the Eclipse Foundation
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * SPDX-License-Identifier: EPL-1.0 OR BSD-3-Clause
 */
package org.eclipse.lyo.client.exception;


@SuppressWarnings("serial")
public final class RootServicesException extends OslcClientApplicationException {

private static final String MESSAGE_KEY = "RootServicesException";

	private final String jazzUrl;


	public RootServicesException(final String jazzUrl, final Exception exception) {
		super(MESSAGE_KEY, new Object[] {jazzUrl}, exception);

		this.jazzUrl = jazzUrl;
	}


	public String getJazzUrl() {
		return jazzUrl;
	}

}
