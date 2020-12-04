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

import java.net.URI;

import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;

@OslcNamespace(OslcConstants.OSLC_CORE_NAMESPACE)
@OslcResourceShape(title = "OSLC OAuth Configuration Resource Shape", describes = OslcConstants.TYPE_O_AUTH_CONFIGURATION)
public class OAuthConfiguration extends AbstractResource {
	private URI authorizationURI;
	private URI oauthAccessTokenURI;
	private URI oauthRequestTokenURI;

	public OAuthConfiguration() {
		super();
	}

	public OAuthConfiguration(final URI oauthRequestTokenURI, final URI authorizationURI,  final URI oauthAccessTokenURI) {
		this();

		this.oauthRequestTokenURI = oauthRequestTokenURI;
		this.authorizationURI =	 authorizationURI;
		this.oauthAccessTokenURI = oauthAccessTokenURI;
	}

	@OslcDescription("URI for obtaining OAuth authorization")
	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "authorizationURI")
	@OslcReadOnly
	@OslcTitle("Authorization URI")
	public URI getAuthorizationURI() {
		return authorizationURI;
	}

	@OslcDescription("URI for obtaining OAuth access token")
	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "oauthAccessTokenURI")
	@OslcReadOnly
	@OslcTitle("Access Token URI")
	public URI getOauthAccessTokenURI() {
		return oauthAccessTokenURI;
	}

	@OslcDescription("URI for obtaining OAuth request token")
	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "oauthRequestTokenURI")
	@OslcReadOnly
	@OslcTitle("Request Token URI")
	public URI getOauthRequestTokenURI() {
		return oauthRequestTokenURI;
	}

	public void setAuthorizationURI(final URI authorizationURI) {
		this.authorizationURI = authorizationURI;
	}

	public void setOauthAccessTokenURI(final URI oauthAccessTokenURI) {
		this.oauthAccessTokenURI = oauthAccessTokenURI;
	}

	public void setOauthRequestTokenURI(final URI oauthRequestTokenURI) {
		this.oauthRequestTokenURI = oauthRequestTokenURI;
	}
}
