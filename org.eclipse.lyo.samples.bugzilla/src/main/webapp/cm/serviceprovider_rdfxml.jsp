<?xml version="1.0" encoding="UTF-8"?>
<%--
 Copyright (c) 2011, 2012 IBM Corporation.

 All rights reserved. This program and the accompanying materials
 are made available under the terms of the Eclipse Public License v1.0
 and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 
 The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 and the Eclipse Distribution License is available at
 http://www.eclipse.org/org/documents/edl-v10.php.
 
 Contributors:
 
    IBM Corporation - initial API and implementation
--%>
<%@ page contentType="application/rdf+xml" language="java" pageEncoding="UTF-8" %>
<%@ page import="java.util.List" %>
<%@ page import="org.eclipse.lyo.samples.bugzilla.jbugzx.base.Product" %>
<%@ page import="org.eclipse.lyo.samples.bugzilla.URLStrategy"%>
<%
String bugzillaUri = (String) request.getAttribute("bugzillaUri");
Product product = (Product)request.getAttribute("product");
response.setHeader("OSLC-Core-Version", "2.0");
%>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
	xmlns:dcterms="http://purl.org/dc/terms/"
	xmlns:oslc="http://open-services.net/ns/core#">

    <oslc:ServiceProvider rdf:about="<%= URLStrategy.getServiceProviderURL(product.getId()) %>">
		<dcterms:title>OSLC-CM Adapter/Bugzilla Service Provider: 
		Product <%= product.getName() + "(" + product.getId() + ")" %></dcterms:title>
		<dcterms:description>
        Enables navigation to OSLC-CM Resource Creator and Selector Dialogs
        </dcterms:description>

		<dcterms:publisher>
			<oslc:Publisher>
				<dcterms:title>OSLC Tools Project</dcterms:title>
				<dcterms:identifier>org.eclipse.lyo.samples.bugzilla.test</dcterms:identifier>
				<oslc:icon rdf:resource="<%= bugzillaUri %>/images/favicon.ico" />
			</oslc:Publisher>
		</dcterms:publisher>

		<oslc:service>
			<oslc:Service>
				<oslc:domain rdf:resource="http://open-services.net/ns/cm#" />
				<oslc:selectionDialog>
					<oslc:Dialog>
						<dcterms:title>Bugzilla Bug Picker</dcterms:title>
						<oslc:label>Bugs</oslc:label>
						<oslc:dialog rdf:resource="<%= URLStrategy.getDelegatedSelectionURL(product.getId()) %>" />
						<oslc:hintHeight>300px</oslc:hintHeight>
						<oslc:hintWidth>350px</oslc:hintWidth>
						<oslc:resourceType rdf:resource="http://open-services.net/ns/cm#ChangeRequest" />
						<oslc:usage rdf:resource="http://open-services.net/ns/core#default" />
					</oslc:Dialog>
				</oslc:selectionDialog>

				<oslc:creationDialog>
					<oslc:Dialog>
						<dcterms:title>Bugzilla Bug Creator</dcterms:title>
						<oslc:label>Bugs</oslc:label>
                        <oslc:dialog rdf:resource="<%= URLStrategy.getDelegatedCreationURL(product.getId()) %>" />
						<oslc:hintHeight>240px</oslc:hintHeight>
						<oslc:hintWidth>420px</oslc:hintWidth>
						<oslc:resourceType rdf:resource="http://open-services.net/ns/cm#ChangeRequest" />
						<oslc:usage rdf:resource="http://open-services.net/ns/core#default" />
					</oslc:Dialog>
				</oslc:creationDialog>

				<oslc:creationFactory>
					<oslc:CreationFactory>
						<dcterms:title>OSLC-CM Creation Factory</dcterms:title>
						<oslc:label>CreationFactory</oslc:label>
						<oslc:creation 
							rdf:resource="<%= URLStrategy.getChangeRequestCollectionURL(product.getId()) %>" />
						<oslc:resourceShape
							rdf:resource="<%= URLStrategy.getCreationShapeURL(product.getId()) %>" />
						<oslc:resourceType
							rdf:resource="http://open-services.net/ns/cm#ChangeRequest" />
						<oslc:usage
							rdf:resource="http://open-services.net/ns/core#default" />
					</oslc:CreationFactory>
				</oslc:creationFactory>
				
				<oslc:queryCapability>
					<oslc:QueryCapability>
						<dcterms:title>OSLC-CM Query Capability</dcterms:title>
						<oslc:label>QueryCapability</oslc:label>
						<oslc:queryBase
							rdf:resource="<%= URLStrategy.getChangeRequestCollectionURL(product.getId()) %>" />
						<oslc:resourceShape
                            rdf:resource="<%= URLStrategy.getQueryShapeURL(product.getId()) %>" />
						<oslc:resourceType
							rdf:resource="http://open-services.net/ns/cm#ChangeRequest" />
						<oslc:usage
							rdf:resource="http://open-services.net/ns/core#default" />
					</oslc:QueryCapability>
				</oslc:queryCapability>

			</oslc:Service>
		</oslc:service>

        <oslc:prefixDefinition>
            <oslc:PrefixDefinition>
                <oslc:prefix>oslc</oslc:prefix>
                <oslc:prefixBase rdf:resource="http://open-services.net/ns/core#" />
            </oslc:PrefixDefinition>
        </oslc:prefixDefinition>
                        
        <oslc:prefixDefinition>
            <oslc:PrefixDefinition>
                <oslc:prefix>oslc_cm</oslc:prefix>
                <oslc:prefixBase rdf:resource="http://open-services.net/ns/cm#" />
            </oslc:PrefixDefinition>
        </oslc:prefixDefinition>
                        
        <oslc:prefixDefinition>
            <oslc:PrefixDefinition>
                <oslc:prefix>rdf</oslc:prefix>
                <oslc:prefixBase rdf:resource="http://www.w3.org/1999/02/22-rdf-syntax-ns#" />
            </oslc:PrefixDefinition>
        </oslc:prefixDefinition>
                        
        <oslc:prefixDefinition>
            <oslc:PrefixDefinition>
                <oslc:prefix>dcterms</oslc:prefix>
                <oslc:prefixBase rdf:resource="http://purl.org/dc/terms/" />
            </oslc:PrefixDefinition>
        </oslc:prefixDefinition>
                        
        <oslc:prefixDefinition>
            <oslc:PrefixDefinition>
                <oslc:prefix>rdfs</oslc:prefix>
                <oslc:prefixBase rdf:resource="http://www.w3.org/2000/01/rdf-schema#" />
            </oslc:PrefixDefinition>
        </oslc:prefixDefinition>
                        
        <oslc:prefixDefinition>
            <oslc:PrefixDefinition>
                <oslc:prefix>foaf</oslc:prefix>
                <oslc:prefixBase rdf:resource="http://xmlns.com/foaf/0.1/" />
            </oslc:PrefixDefinition>
        </oslc:prefixDefinition>
   
        <oslc:prefixDefinition>
            <oslc:PrefixDefinition>
                <oslc:prefix>bugz</oslc:prefix>
                <oslc:prefixBase rdf:resource="http://www.bugzilla.org/rdf#" />
            </oslc:PrefixDefinition>
        </oslc:prefixDefinition>
 
	</oslc:ServiceProvider>

</rdf:RDF>
