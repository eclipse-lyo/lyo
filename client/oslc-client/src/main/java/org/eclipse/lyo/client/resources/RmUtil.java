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
package org.eclipse.lyo.client.resources;

import net.oauth.OAuthException;
import org.eclipse.lyo.client.OslcClient;
import org.eclipse.lyo.client.exception.ResourceNotFoundException;
import org.eclipse.lyo.client.OSLCConstants;
import org.eclipse.lyo.oslc4j.core.model.CreationFactory;
import org.eclipse.lyo.oslc4j.core.model.ResourceShape;
import org.eclipse.lyo.oslc4j.core.model.Service;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;

import javax.ws.rs.core.Response;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

@Deprecated
public final class RmUtil {
	public static ResourceShape lookupRequirementsInstanceShapes(final String serviceProviderUrl, final String oslcDomain, final String oslcResourceType, OslcClient client, String requiredInstanceShape)
			throws IOException, URISyntaxException, ResourceNotFoundException, OAuthException{
		return lookupRequirementsInstanceShapes(serviceProviderUrl, oslcDomain, oslcResourceType, client, requiredInstanceShape,null);
	}
	public static ResourceShape lookupRequirementsInstanceShapes(final String serviceProviderUrl, final String oslcDomain, final String oslcResourceType, OslcClient client, String requiredInstanceShape, String configurationContext)
			throws IOException, URISyntaxException, ResourceNotFoundException, OAuthException
	{

		Response response = client.getResource(serviceProviderUrl,null, OSLCConstants.CT_RDF, configurationContext);
		ServiceProvider serviceProvider = response.readEntity(ServiceProvider.class);
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
											response = client.getResource(typeURI.toString(), null, OSLCConstants.CT_RDF, configurationContext);
											ResourceShape resourceShape =  response.readEntity(ResourceShape.class);
											String typeTitle = resourceShape.getTitle();
											String typeAbout = resourceShape.getAbout().toString();
											if ( ( typeTitle != null) && (typeTitle.equalsIgnoreCase(requiredInstanceShape)) ||
													(typeAbout != null && typeAbout.equalsIgnoreCase(requiredInstanceShape)) ) {
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
}
