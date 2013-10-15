/*******************************************************************************
 * Copyright (c) 2011,2013 IBM Corporation.
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
 *     Masaki Wakao 
 *     Yoshio Horiuchi 
 *     Kohji Ohsawa 
 *******************************************************************************/
package org.eclipse.lyo.samples.excel.services.common;

import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.UriInfo;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.eclipse.lyo.rio.core.IConstants;
import org.eclipse.lyo.rio.services.RioServiceException;
import org.eclipse.lyo.rio.store.RioStore;
import org.eclipse.lyo.rio.util.XmlUtils;
import org.eclipse.lyo.samples.excel.common.ConfigSingleton;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

@Path(IConstants.SERVICE_SERVICES + "/catalog/{projectId}")
public class ServiceProviderService {
	private static final long serialVersionUID = 1889321991789915986L;
	
	private String baseUrl;
	
	@GET
	@Produces ({"application/rdf+xml", "application/xml"})
	public String getServiceProvider(
			@Context UriInfo uriInfo, 
			@PathParam("projectId") String projectId) throws RioServiceException {
		baseUrl = uriInfo.getBaseUri().toString();
		String about = uriInfo.getAbsolutePath().toString();
		String context = uriInfo.getBaseUri() + IConstants.SERVICE_SERVICES + "/" + projectId;
		return buildServicesResource(about, context);
	}

	private String getBaseUrl() {
		return baseUrl;
	}
	
	public String buildServicesResource(String about, String context) throws RioServiceException{
		
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setValidating(true);
		factory.setNamespaceAware(true);
		try{
			DocumentBuilder builder = factory.newDocumentBuilder();
			Document doc = builder.newDocument();
			Element rdf = doc.createElementNS(IConstants.RDF_NAMESPACE, IConstants.RDF_TYPE_PTERM_RDF);
			doc.appendChild(rdf);
			rdf.setAttribute("xmlns:" + IConstants.RDF_PREFIX, IConstants.RDF_NAMESPACE); //$NON-NLS-1$
			rdf.setAttribute("xmlns:" + IConstants.DCTERMS_PREFIX, IConstants.DCTERMS_NAMESPACE); //$NON-NLS-1$
			rdf.setAttribute("xmlns:" + IConstants.OSLC_PREFIX, IConstants.OSLC_NAMESPACE); //$NON-NLS-1$
			
			Element sp = doc.createElementNS(IConstants.OSLC_NAMESPACE, IConstants.OSLC_TYPE_PTERM_SERVICEPROVIDER);
			rdf.appendChild(sp);
			sp.setAttributeNS(IConstants.RDF_NAMESPACE, IConstants.RDF_PTERM_ABOUT, about);
			
			Element elmTitle = doc.createElementNS(IConstants.DCTERMS_NAMESPACE, IConstants.DCTERMS_PTERM_TITLE);
			elmTitle.setTextContent("OSLC Excel Adapter Catalog");
			sp.appendChild(elmTitle);
			
			Element elmDescr = doc.createElementNS(IConstants.DCTERMS_NAMESPACE, IConstants.DCTERMS_PTERM_DESCRIPTION);
			elmDescr.setTextContent("OSLC Excel Adapter for Change Management Service Document");
			sp.appendChild(elmDescr);
			
			appendPublisher(sp);
			
			Map<String, String> predefinedMappings = RioStore.getPredefinedNamespaceMappings();
			Set<Entry<String, String>> entries = predefinedMappings.entrySet();
			for (Entry<String, String> prefixMapping : entries) {
				appendPrefixDefinition(sp, prefixMapping.getKey(), prefixMapping.getValue());
			}
			// append custom prefixes
			ConfigSingleton config = ConfigSingleton.getInstance();
			Map<String, String> nsPrefixes = config.getNsPrefixes();
			entries = nsPrefixes.entrySet();
			for (Entry<String, String> nsMapping : entries) {
				String ns = nsMapping.getValue();
				if(!predefinedMappings.containsKey(ns)){
					appendPrefixDefinition(sp, ns, nsMapping.getKey());
				}
			}
			
			Element service1 = doc.createElementNS(IConstants.OSLC_NAMESPACE, IConstants.OSLC_PTERM_SERVICE);
			sp.appendChild(service1);
			
			// add default generic services
			Element service2 = doc.createElementNS(IConstants.OSLC_NAMESPACE, IConstants.OSLC_TYPE_PTERM_SERVICE);
			service1.appendChild(service2);

			Element domain = doc.createElementNS(IConstants.OSLC_NAMESPACE, IConstants.OSLC_PTERM_DOMAIN);
			domain.setAttributeNS(IConstants.RDF_NAMESPACE, IConstants.RDF_PTERM_RESOURCE, config.getServiceDomainDefault());
			service2.appendChild(domain);
			
			String resShapeUrl = null;
			Map<String, String>queryCapabilities = config.getQueryCapabilities();
			for(String uriLastSegment: queryCapabilities.keySet()){
				String title = "Query for " + uriLastSegment;
				appendQueryCapability(service2, title, title, context + "/query/" + uriLastSegment, 
						queryCapabilities.get(uriLastSegment), resShapeUrl, IConstants.OSLC_DEFAULT);
			}

			return XmlUtils.prettyPrint(doc);
		} catch (Exception e) {
			throw new RioServiceException(IConstants.SC_INTERNAL_ERROR, "Unable to construct service document");
		} 
		
	}
	
	private void appendQueryCapability(Element elm, String title, String label, String url, String rdfType, 
			String shapeUrl, String usage) {
		
		Document doc = elm.getOwnerDocument();
		Element qc1 = doc.createElementNS(IConstants.OSLC_NAMESPACE, IConstants.OSLC_PTERM_QUERYCAPABILITY);
		elm.appendChild(qc1);
		Element qc2 = doc.createElementNS(IConstants.OSLC_NAMESPACE, IConstants.OSLC_TYPE_PTERM_QUERYCAPABILITY);
		qc1.appendChild(qc2);
		
		Element elmTitle = doc.createElementNS(IConstants.DCTERMS_NAMESPACE, IConstants.DCTERMS_PTERM_TITLE);
		qc2.appendChild(elmTitle);
		elmTitle.setTextContent(title);

		Element elmLabel = doc.createElementNS(IConstants.OSLC_NAMESPACE, IConstants.OSLC_PTERM_LABEL);
		qc2.appendChild(elmLabel);
		elmLabel.setTextContent(label);

		Element elmQueryBase = doc.createElementNS(IConstants.OSLC_NAMESPACE, IConstants.OSLC_PTERM_QUERYBASE);
		qc2.appendChild(elmQueryBase);
		elmQueryBase.setAttributeNS(IConstants.RDF_NAMESPACE, IConstants.RDF_PTERM_RESOURCE, url);
		
		Element resourceType = doc.createElementNS(IConstants.OSLC_NAMESPACE, IConstants.OSLC_PTERM_RESOURCETYPE);
		resourceType.setAttributeNS(IConstants.RDF_NAMESPACE, IConstants.RDF_PTERM_RESOURCE, rdfType);
		qc2.appendChild(resourceType);

		if( shapeUrl != null ) {
			Element elmShape = doc.createElementNS(IConstants.OSLC_NAMESPACE, IConstants.OSLC_PTERM_RESOURCESHAPE);
			qc2.appendChild(elmShape);
			elmShape.setAttributeNS(IConstants.RDF_NAMESPACE, IConstants.RDF_PTERM_RESOURCE, shapeUrl);
		}
		
		if( usage != null ) {
			Element elmUsage = doc.createElementNS(IConstants.OSLC_NAMESPACE, IConstants.OSLC_PTERM_USAGE);
			qc2.appendChild(elmUsage);
			elmUsage.setAttributeNS(IConstants.RDF_NAMESPACE, IConstants.RDF_PTERM_RESOURCE, usage);
		}
		
	}
	
	private void appendPrefixDefinition( Element sp, String namespace, String prefix ) {
		Document doc = sp.getOwnerDocument();
		Element pd1 = doc.createElementNS(IConstants.OSLC_NAMESPACE, IConstants.OSLC_PTERM_PREFIXDEFINITION);
		sp.appendChild(pd1);
		Element pd2 = doc.createElementNS(IConstants.OSLC_NAMESPACE, IConstants.OSLC_TYPE_PTERM_PREFIXDEFINITION);
		pd1.appendChild(pd2);
		
		Element elmPrefix = doc.createElementNS(IConstants.OSLC_NAMESPACE, IConstants.OSLC_PTERM_PREFIX);
		pd2.appendChild(elmPrefix);
		elmPrefix.setTextContent(prefix);

		Element elmPrefixBase = doc.createElementNS(IConstants.OSLC_NAMESPACE, IConstants.OSLC_PTERM_PREFIXBASE);
		pd2.appendChild(elmPrefixBase);
		elmPrefixBase.setTextContent(namespace);
	}
	
	private void appendPublisher(Element sp) throws RioServiceException {
		Document doc = sp.getOwnerDocument();
		Element dcPub = doc.createElementNS(IConstants.DCTERMS_NAMESPACE, IConstants.DCTERMS_PTERM_PUBLISHER);
		sp.appendChild(dcPub);
		
		Element oslcPub = doc.createElementNS(IConstants.OSLC_NAMESPACE, IConstants.OSLC_PTERM_PUBLISHER);
		dcPub.appendChild(oslcPub);
		
		Element elm = doc.createElementNS(IConstants.DCTERMS_NAMESPACE, IConstants.DCTERMS_PTERM_TITLE);
		oslcPub.appendChild(elm);
		ConfigSingleton config = ConfigSingleton.getInstance();
		elm.setTextContent(config.getPublisherTitle());
		
		elm = doc.createElementNS(IConstants.DCTERMS_NAMESPACE, IConstants.DCTERMS_PTERM_IDENTIFIER);
		oslcPub.appendChild(elm);
		elm.setTextContent(config.getPublisherIdentifier());
		
		elm = doc.createElementNS(IConstants.OSLC_NAMESPACE, IConstants.OSLC_PTERM_ICON);
		oslcPub.appendChild(elm);
		String iconUrl = getBaseUrl() + config.getPublisherIcon();
		elm.setAttributeNS(IConstants.RDF_NAMESPACE, IConstants.RDF_PTERM_RESOURCE, iconUrl);
	}

	
}
