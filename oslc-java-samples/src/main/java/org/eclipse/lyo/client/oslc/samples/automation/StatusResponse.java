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

import java.util.logging.Logger;

import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.core.model.Occurs;
import org.eclipse.lyo.oslc4j.core.model.ValueType;

@OslcResourceShape(title = "Status Response Resource Shape", describes = IConstants.TYPE_STATUS_RESPONSE)
@OslcNamespace(IConstants.NAMESPACE_URI_JAZZ_AUTO_RQM)
public class StatusResponse extends AbstractResource implements IConstants {

	private static final Logger logger = Logger.getLogger(StatusResponse.class.getName());

	/**
	 * Starting value for normal informational status message
	 */
	public static final int STATUS_INFORMATIONAL = 100;

	/**
	 * Starting value for success messages
	 */
	public static final int STATUS_OK            = 200;

	/**
	 * Starting value for warning messages
	 */
	public static final int STATUS_WARNING       = 300;

	/**
	 * Starting value for error messages
	 */
	public static final int STATUS_ERROR         = 400;

	private int statusCode;

	private String status;

	/**
	 * Create a new StatusResponse object with the provided statusCode and
	 * status message. The expected range is 100-599.
	 *
	 * @param statusCode
	 * @param status
	 * @see @link {@link StatusResponse#STATUS_INFORMATIONAL}
	 * @see @link {@link StatusResponse#STATUS_OK}
	 * @see @link {@link StatusResponse#STATUS_WARNING}
	 * @see @link {@link StatusResponse#STATUS_ERROR}
	 */
	public StatusResponse(int statusCode, String status) {
		this.status = status;
		setStatusCode(statusCode);
	}

	@OslcDescription("The severity of the status")
	@OslcPropertyDefinition(NAMESPACE_URI_JAZZ_AUTO_RQM + "statusCode")
	@OslcTitle("Status Code")
    @OslcOccurs(Occurs.ExactlyOne)
	@OslcValueType(ValueType.Integer)
	public int getStatusCode() {
		return statusCode;
	}

	/**
	 * Set the status code for this response. The expected range is 100-599.
	 *
	 * @param statusCode
	 * @see @link {@link StatusResponse#STATUS_INFORMATIONAL}
	 * @see @link {@link StatusResponse#STATUS_OK}
	 * @see @link {@link StatusResponse#STATUS_WARNING}
	 * @see @link {@link StatusResponse#STATUS_ERROR}
	 */
	public void setStatusCode(int statusCode) {
		this.statusCode = statusCode;

		if (statusCode < 100 || statusCode > 599) {

			logger.warning("status code not in expected range: " + statusCode);

		}
	}

	@OslcDescription("Human friendly description of the status")
	@OslcPropertyDefinition(NAMESPACE_URI_JAZZ_AUTO_RQM + "status")
	@OslcTitle("Status")
    @OslcOccurs(Occurs.ZeroOrOne)
	@OslcValueType(ValueType.String)
	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

}
