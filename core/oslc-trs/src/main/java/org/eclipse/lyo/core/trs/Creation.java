/*
 * Copyright (c) 2020 Contributors to the Eclipse Foundation
 *
 * See the NOTICE file(s) distributed with this work for additional
 * information regarding copyright ownership.
 *
 * This program and the accompanying materials are made available under the
 * terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0, or the Eclipse Distribution License 1.0
 * which is available at http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Simple
 */
package org.eclipse.lyo.core.trs;

import java.net.URI;

import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;

import static org.eclipse.lyo.core.trs.TRSConstants.*;

@OslcNamespace(TRS_NAMESPACE)
@OslcResourceShape(title = "Creation  Shape", describes = TRS_TYPE_CREATION)
public class Creation extends ChangeEvent {

	public Creation() {}
	
	/**
	 * @param about
	 * @param changed
	 * @param order
	 */
	public Creation(URI about, URI changed, int order) {
		super(about, changed, order);
	}
}
