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
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Clause
 */
package org.eclipse.lyo.oslc4j.core.model;

import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRange;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRepresentation;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;

@OslcNamespace(OslcConstants.OSLC_CORE_NAMESPACE)
@OslcResourceShape(title = "OSLC Error Resource Shape", describes = OslcConstants.TYPE_ERROR)
public class Error {
	private ExtendedError extendedError;
	private String		  message;
	private String		  statusCode;

	public Error() {
		super();
	}

	@OslcDescription("Extended error information.")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "extendedError")
	@OslcRange(OslcConstants.TYPE_EXTENDED_ERROR)
	@OslcReadOnly
	@OslcRepresentation(Representation.Inline)
	@OslcTitle("Extended Error")
	@OslcValueShape(OslcConstants.PATH_RESOURCE_SHAPES + "/" + OslcConstants.PATH_EXTENDED_ERROR)
	@OslcValueType(ValueType.LocalResource)
	public ExtendedError getExtendedError() {
		return extendedError;
	}

	@OslcDescription("An informative message describing the error that occurred.")
	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "message")
	@OslcReadOnly
	@OslcTitle("Message")
	public String getMessage() {
		return message;
	}

	@OslcDescription("The HTTP status code reported with the error.")
	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "statusCode")
	@OslcReadOnly
	@OslcTitle("Status Code")
	public String getStatusCode() {
		return statusCode;
	}

	public void setExtendedError(final ExtendedError extendedError) {
		this.extendedError = extendedError;
	}

	public void setMessage(final String message) {
		this.message = message;
	}

	public void setStatusCode(final String statusCode) {
		this.statusCode = statusCode;
	}
}
