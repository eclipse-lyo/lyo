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

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.UriInfo;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.eclipse.lyo.rio.core.IConstants;
import org.eclipse.lyo.rio.services.RioServiceException;
import org.eclipse.lyo.rio.util.XmlUtils;
import org.eclipse.lyo.samples.excel.adapter.common.AdapterRegistry;
import org.eclipse.lyo.samples.excel.adapter.common.ResourceAdapter;
import org.eclipse.lyo.samples.excel.common.ConfigSingleton;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

@Path("catalog")
public class CatalogService {
	//TODO
	public static final String OSLC_PTERM_PREFIXDEFINITION2 = IConstants.OSLC_PREFIX + ":PrefixDefinition";  
	
	private String baseUrl;

	@GET
	@Produces ({"application/rdf+xml", "application/xml"})
	public String getCatalog(@Context UriInfo uriInfo) throws RioServiceException {
		baseUrl = uriInfo.getBaseUri().toString();
		String about = uriInfo.getAbsolutePath().toString();
		return buildServicesResource(about);
	}
	
	private String getBaseUrl() {
		return baseUrl;
	}

	public String buildServicesResource(String about) throws RioServiceException {
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
			
			Element sp = doc.createElementNS(IConstants.OSLC_NAMESPACE, IConstants.OSLC_TYPE_PTERM_SERVICEPROVIDERCATALOG);
			rdf.appendChild(sp);
			sp.setAttributeNS(IConstants.RDF_NAMESPACE, IConstants.RDF_PTERM_ABOUT, about);
			
			Element elmTitle = doc.createElementNS(IConstants.DCTERMS_NAMESPACE, IConstants.DCTERMS_PTERM_TITLE);
			elmTitle.setTextContent("OSLC Excel Adapter Catalog");
			sp.appendChild(elmTitle);
			
			Element elmDescr = doc.createElementNS(IConstants.DCTERMS_NAMESPACE, IConstants.DCTERMS_PTERM_DESCRIPTION);
			elmDescr.setTextContent("Catalog for OSLC Excel Adapter");
			sp.appendChild(elmDescr);

			appendPublisher(sp);

			Map<String, String> predefinedMappings = getPredefinedNamespaceMappings();
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

			// service resource
			ResourceAdapter adapter = AdapterRegistry.getAdapter(getBaseUrl() + IConstants.SERVICE_SERVICES);
			adapter.loadRepository();
			List<String> providers = adapter.getContexts();
			for(String providerId: providers){
				Element service = doc.createElementNS(IConstants.OSLC_NAMESPACE, IConstants.OSLC_PTERM_SERVICEPROVIDER);
				String svcUri = getBaseUrl() + IConstants.SERVICE_SERVICES + "/catalog/" + providerId;
				Element serviceProvider = doc.createElementNS(IConstants.OSLC_NAMESPACE, IConstants.OSLC_TYPE_PTERM_SERVICEPROVIDER);
				serviceProvider.setAttributeNS(IConstants.RDF_NAMESPACE, IConstants.RDF_PTERM_ABOUT, svcUri);
				Element ‚”itle = doc.createElementNS(IConstants.DCTERMS_NAMESPACE, IConstants.DCTERMS_PTERM_TITLE);
				‚”itle.setTextContent("OSLC Excel Adapter");
				serviceProvider.appendChild(‚”itle);
				service.appendChild(serviceProvider);
				sp.appendChild(service);
			}
			
			return XmlUtils.prettyPrint(doc);
		} catch (Exception e) {
			throw new RioServiceException(e);
		} finally {
			
		}
	}

	private Map<String, String> getPredefinedNamespaceMappings() {
		return new HashMap<String, String>();
	}

	private void appendPrefixDefinition( Element sp, String namespace, String prefix ) {
		Document doc = sp.getOwnerDocument();
		Element pd1 = doc.createElementNS(IConstants.OSLC_NAMESPACE, IConstants.OSLC_PTERM_PREFIXDEFINITION);
		sp.appendChild(pd1);
		Element pd2 = doc.createElementNS(IConstants.OSLC_NAMESPACE, OSLC_PTERM_PREFIXDEFINITION2);
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
