/*******************************************************************************
 * Copyright (c) 2012, 2013 IBM Corporation.
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
 *	   Russell Boykin		- initial API and implementation
 *	   Alberto Giammaria	- initial API and implementation
 *	   Chris Peters			- initial API and implementation
 *	   Gianluca Bernardini	- initial API and implementation
 *	   Samuel Padgett		- remove final from class
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.core.model;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRange;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRepresentation;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;

@OslcNamespace(OslcConstants.OSLC_CORE_NAMESPACE)
@OslcResourceShape(title = "OSLC Service Provider Catalog Resource Shape", describes = OslcConstants.TYPE_SERVICE_PROVIDER_CATALOG)
public class ServiceProviderCatalog extends AbstractResource {
	private final SortedSet<URI> domains = new TreeSet<URI>();
	private final SortedSet<URI> referencedServiceProviderCatalogs = new TreeSet<URI>();
	private final List<ServiceProvider> serviceProviders = new ArrayList<ServiceProvider>();

	private String description;
	private OAuthConfiguration oauthConfiguration;
	private Publisher publisher;
	private String title;

	public ServiceProviderCatalog() {
		super();
	}

	public void addDomain(final URI domain) {
		this.domains.add(domain);
	}

	public void addDomains(final Collection<URI> domains) {
		for (final URI domain : domains) {
			addDomain(domain);
		}
	}

	public void addServiceProvider(final ServiceProvider serviceProvider) {
		this.serviceProviders.add(serviceProvider);
	}

	@OslcDescription("Description of the service provider catalog")
	@OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "description")
	@OslcReadOnly
	@OslcTitle("Description")
	@OslcValueType(ValueType.XMLLiteral)
	public String getDescription() {
		return description;
	}

	@OslcDescription("URIs of the OSLC domain specifications that may be implemented by referenced services")
	@OslcName("domain")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "domain")
	@OslcReadOnly
	@OslcTitle("Domains")
	public URI[] getDomains() {
		return domains.toArray(new URI[domains.size()]);
	}

	@OslcDescription("Defines the three OAuth URIs required for a client to act as an OAuth consumer")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "oauthConfiguration")
	@OslcRange(OslcConstants.TYPE_O_AUTH_CONFIGURATION)
	@OslcReadOnly
	@OslcRepresentation(Representation.Inline)
	@OslcTitle("OAuth URIs")
	@OslcValueShape(OslcConstants.PATH_RESOURCE_SHAPES + "/" + OslcConstants.PATH_OAUTH_CONFIGURATION)
	@OslcValueType(ValueType.LocalResource)
	public OAuthConfiguration getOauthConfiguration() {
		return oauthConfiguration;
	}

	@OslcDescription("Describes the software product that provides the implementation")
	@OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "publisher")
	@OslcRange(OslcConstants.TYPE_PUBLISHER)
	@OslcReadOnly
	@OslcRepresentation(Representation.Inline)
	@OslcTitle("Publisher")
	@OslcValueShape(OslcConstants.PATH_RESOURCE_SHAPES + "/" + OslcConstants.PATH_PUBLISHER)
	@OslcValueType(ValueType.LocalResource)
	public Publisher getPublisher() {
		return publisher;
	}

	@OslcDescription("Additional service provider catalogs")
	@OslcName("serviceProviderCatalog")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "serviceProviderCatalog")
	@OslcRange(OslcConstants.TYPE_SERVICE_PROVIDER_CATALOG)
	@OslcReadOnly
	@OslcTitle("Additional Service Provider Catalogs")
	@OslcValueShape(OslcConstants.PATH_RESOURCE_SHAPES + "/" + OslcConstants.PATH_SERVICE_PROVIDER_CATALOG)
	public URI[] getReferencedServiceProviderCatalogs() {
		return referencedServiceProviderCatalogs.toArray(new URI[referencedServiceProviderCatalogs.size()]);
	}

	@OslcDescription("Service providers")
	@OslcName("serviceProvider")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "serviceProvider")
	@OslcRange(OslcConstants.TYPE_SERVICE_PROVIDER)
	@OslcReadOnly
	@OslcRepresentation(Representation.Inline)
	@OslcTitle("Service Providers")
	@OslcValueShape(OslcConstants.PATH_RESOURCE_SHAPES + "/" + OslcConstants.PATH_SERVICE_PROVIDER)
	@OslcValueType(ValueType.LocalResource)
	public ServiceProvider[] getServiceProviders() {
		return serviceProviders.toArray(new ServiceProvider[serviceProviders.size()]);
	}

	@OslcDescription("Title of the service provider catalog")
	@OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "title")
	@OslcReadOnly
	@OslcTitle("Title")
	@OslcValueType(ValueType.XMLLiteral)
	public String getTitle() {
		return title;
	}

	public void removeDomain(final URI domain) {
		domains.remove(domain);
	}

	public void removeDomains(final Collection<URI> domains) {
		for (final URI domain : domains) {
			removeDomain(domain);
		}
	}

	public void removeServiceProvider(final ServiceProvider serviceProvider) {
		serviceProviders.remove(serviceProvider);
	}

	public void setDescription(final String description) {
		this.description = description;
	}

	public void setDomains(final URI[] domains) {
		this.domains.clear();
		if (domains != null) {
			this.domains.addAll(Arrays.asList(domains));
		}
	}

	public void setOauthConfiguration(final OAuthConfiguration oauthConfiguration) {
		this.oauthConfiguration = oauthConfiguration;
	}

	public void setPublisher(final Publisher publisher) {
		this.publisher = publisher;
	}

	public void setReferencedServiceProviderCatalogs(final URI[] referencedServiceProviderCatalogs) {
		this.referencedServiceProviderCatalogs.clear();
		if (referencedServiceProviderCatalogs != null) {
			this.referencedServiceProviderCatalogs.addAll(Arrays.asList(referencedServiceProviderCatalogs));
		}
	}

	public void setServiceProviders(final ServiceProvider[] serviceProviders) {
		this.serviceProviders.clear();
		if (serviceProviders != null) {
			this.serviceProviders.addAll(Arrays.asList(serviceProviders));
		}
	}

	public void setTitle(final String title) {
		this.title = title;
	}
}
