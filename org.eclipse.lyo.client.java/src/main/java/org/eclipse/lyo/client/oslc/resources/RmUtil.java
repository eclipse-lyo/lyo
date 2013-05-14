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
 *     Gabriel Ruelas       - initial API and implementation
 *     Carlos A Arreola     - initial API and implementation
 *******************************************************************************/
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
