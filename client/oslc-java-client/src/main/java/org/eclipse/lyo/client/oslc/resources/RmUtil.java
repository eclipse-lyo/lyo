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
package org.eclipse.lyo.client.oslc.resources;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import net.oauth.OAuthException;

import org.apache.wink.client.ClientResponse;
import org.eclipse.lyo.client.exception.ResourceNotFoundException;
import org.eclipse.lyo.client.oslc.OSLCConstants;
import org.eclipse.lyo.client.oslc.OslcClient;
import org.eclipse.lyo.oslc4j.core.model.CreationFactory;
import org.eclipse.lyo.oslc4j.core.model.ResourceShape;
import org.eclipse.lyo.oslc4j.core.model.Service;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

@Deprecated
public final class RmUtil {

	public static ResourceShape lookupRequirementsInstanceShapes(final String serviceProviderUrl, final String oslcDomain, final String oslcResourceType, OslcClient client, String requiredInstanceShape)
			throws IOException, OAuthException, URISyntaxException, ResourceNotFoundException
	{

		ClientResponse response = client.getResource(serviceProviderUrl,OSLCConstants.CT_RDF);
		ServiceProvider serviceProvider = response.getEntity(ServiceProvider.class);

		if (serviceProvider != null) {
			for (Service service:serviceProvider.getServices()) {
				URI domain = service.getDomain();
				if (domain != null  && domain.toString().equals(oslcDomain)) {
					CreationFactory [] creationFactories = service.getCreationFactories();
					if (creationFactories != null && creationFactories.length > 0) {
						for (CreationFactory creationFactory:creationFactories) {
							for (URI resourceType:creationFactory.getResourceTypes()) {
								if (resourceType.toString() != null && resourceType.toString().equals(oslcResourceType)) {
									URI[] instanceShapes = creationFactory.getResourceShapes();
									if (instanceShapes != null ){
										for ( URI typeURI : instanceShapes) {
											response = client.getResource(typeURI.toString(),OSLCConstants.CT_RDF);
											ResourceShape resourceShape =  response.getEntity(ResourceShape.class);
											String typeTitle = resourceShape.getTitle();
											if ( ( typeTitle != null) && (typeTitle.equalsIgnoreCase(requiredInstanceShape)) ) {
												return resourceShape;
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}


		throw new ResourceNotFoundException(serviceProviderUrl, "InstanceShapes");
	}



	public static Element convertStringToHTML(String text) throws ParserConfigurationException {

		Document document = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();
		Element divElement = document.createElementNS(RmConstants.NAMESPACE_URI_XHTML, "div");
		divElement.setTextContent(text);
		return divElement;
	}

}
