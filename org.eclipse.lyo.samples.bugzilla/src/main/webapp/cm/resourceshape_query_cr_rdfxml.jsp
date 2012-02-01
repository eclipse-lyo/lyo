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
<%@ page contentType="application/x-oslc-compact+xml" language="java" pageEncoding="UTF-8" %>
<%@ page import="java.util.List" %>
<%@ page import="com.j2bugzilla.base.Bug" %>
<%
    String bugzillaUri = (String) request.getAttribute("bugzillaUri");
    List<String> operatingSystems = (List<String>) request.getAttribute("operatingSystems");
    List<String> components = (List<String>) request.getAttribute("components");
    List<String> platforms = (List<String>) request.getAttribute("platforms");
    List<String> versions = (List<String>) request.getAttribute("versions");
%>
<oslc:ResourceShape xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                    xmlns:dc="http://purl.org/dc/terms/" 
                    xmlns:oslc="http://open-services.net/ns/core#"
                    xmlns:oslc_cm="http://open-services.net/ns/cm#"
                    rdf:about="http://example.com/resource/ChangeRequestShape4Reporting">

    <oslc:describes rdf:resource="http://open-services.net/ns/cm#ChangeRequest" />

    <dc:title>Resource Shape for Change Request Creation</dc:title>

    <oslc:property>
        <oslc:Property>
            <oslc:name>title</oslc:name>
            <oslc:propertyDefinition rdf:resource="http://purl.org/dc/terms/title" />
            <oslc:valueType rdf:resource="http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral" />
            <oslc:occurs rdf:resource="http://open-services.net/ns/core#Exactly-one" />
        </oslc:Property>
    </oslc:property>

    <oslc:property>
        <oslc:Property>
            <oslc:name>version</oslc:name>
            <oslc:propertyDefinition rdf:resource="http://www.bugzilla.org/rdf#version" />
            <oslc:valueType rdf:resource="http://www.w3.org/2001/XMLSchema#string" />
            <oslc:occurs rdf:resource="http://open-services.net/ns/core#Exactly-one" />
            <% for (String v : versions) {%><oslc:allowedValue><%= v%></oslc:allowedValue>
            <% }%>
        </oslc:Property>
    </oslc:property>

    <oslc:property>
        <oslc:Property>
            <oslc:name>component</oslc:name>
            <oslc:propertyDefinition rdf:resource="http://www.bugzilla.org/rdf#version" />
            <oslc:valueType rdf:resource="http://www.w3.org/2001/XMLSchema#string" />
            <oslc:occurs rdf:resource="http://open-services.net/ns/core#Exactly-one" />
            <% for (String c : components) {%><oslc:allowedValue><%= c%></oslc:allowedValue>
            <% }%>
        </oslc:Property>
    </oslc:property>

    <oslc:property>
        <oslc:Property>
            <oslc:name>platform</oslc:name>
            <oslc:propertyDefinition rdf:resource="http://www.bugzilla.org/rdf#platform" />
            <oslc:valueType rdf:resource="http://www.w3.org/2001/XMLSchema#string" />
            <oslc:occurs rdf:resource="http://open-services.net/ns/core#Exactly-one" />
            <% for (String p : platforms) {%><oslc:allowedValue><%= p%></oslc:allowedValue>
            <% }%>
        </oslc:Property>
    </oslc:property>

    <oslc:property>
        <oslc:Property>
            <oslc:name>opsys</oslc:name>
            <oslc:propertyDefinition rdf:resource="http://www.bugzilla.org/rdf#opsys" />
            <oslc:valueType rdf:resource="http://www.w3.org/2001/XMLSchema#string" />
            <oslc:occurs rdf:resource="http://open-services.net/ns/core#Exactly-one" />
            <% for (String o : operatingSystems) {%><oslc:allowedValue><%= o%></oslc:allowedValue>
            <% }%>
        </oslc:Property>
    </oslc:property>

    <oslc:property>
        <oslc:Property>
            <oslc:propertyDefinition rdf:resource="http://purl.org/dc/terms/identifier"/>
            <oslc:name>dcterms:identifier</oslc:name>
            <dcterms:title xml:lang="en">Identifier</dcterms:title>
            <oslc:valueType rdf:resource="http://www.w3.org/2001/XMLSchema#string"/>
            <oslc:occurs rdf:resource="http://open-services.net/ns/core#Exactly-one"/>
            <oslc:readOnly rdf:datatype="http://www.w3.org/2001/XMLSchema#boolean">true</oslc:readOnly>
        </oslc:Property>
    </oslc:property>

    <oslc:property>
        <oslc:Property>
            <oslc:propertyDefinition rdf:resource="http://purl.org/dc/terms/contributor"/>
            <oslc:name>dcterms:contributor</oslc:name>
            <dcterms:title xml:lang="en">Contributor</dcterms:title>
            <oslc:valueType rdf:resource="http://open-services.net/ns/core#Resource"/>
            <oslc:range rdf:resource="http://xmlns.com/foaf/0.1/Person"/>
            <oslc:representation rdf:resource="http://open-service.net/ns/core#Inline"/>
            <oslc:occurs rdf:resource="http://open-services.net/ns/core#Exactly-one"/>
            <oslc:readOnly rdf:datatype="http://www.w3.org/2001/XMLSchema#boolean">true</oslc:readOnly>
        </oslc:Property>
    </oslc:property>

    <oslc:property>
        <oslc:Property>
            <oslc:propertyDefinition rdf:resource="http://open-services.net/ns/cm#status"/>
            <oslc:name>oslc_cm:status</oslc:name>
            <dcterms:title xml:lang="en">Identifier</dcterms:title>
            <oslc:valueType rdf:resource="http://www.w3.org/2001/XMLSchema#string"/>
            <oslc:occurs rdf:resource="http://open-services.net/ns/core#Exactly-one"/>				
        </oslc:Property>
    </oslc:property>

    <oslc:property>
        <oslc:Property>
            <oslc:propertyDefinition rdf:resource="http://purl.org/dc/terms/created"/>
            <oslc:name>dcterms:created</oslc:name>
            <dcterms:title xml:lang="en">Date Created</dcterms:title>
            <oslc:valueType rdf:resource="http://www.w3.org/2001/XMLSchema#dateTime"/>
            <oslc:occurs rdf:resource="http://open-services.net/ns/core#Exactly-one"/>
            <oslc:readOnly rdf:datatype="http://www.w3.org/2001/XMLSchema#boolean">true</oslc:readOnly>
        </oslc:Property>
    </oslc:property>

    <oslc:property>
        <oslc:Property>
            <oslc:propertyDefinition rdf:resource="http://purl.org/dc/terms/modified"/>
            <oslc:name>dcterms:modified</oslc:name>
            <dcterms:title xml:lang="en">Date Modified</dcterms:title>
            <oslc:valueType rdf:resource="http://www.w3.org/2001/XMLSchema#dateTime"/>
            <oslc:occurs rdf:resource="http://open-services.net/ns/core#Exactly-one"/>
            <oslc:readOnly rdf:datatype="http://www.w3.org/2001/XMLSchema#boolean">true</oslc:readOnly>
        </oslc:Property>
    </oslc:property>

</oslc:ResourceShape>
