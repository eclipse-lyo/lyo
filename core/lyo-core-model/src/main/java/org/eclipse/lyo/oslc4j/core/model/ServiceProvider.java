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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;
import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRange;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRepresentation;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;

@OslcNamespace(OslcConstants.OSLC_CORE_NAMESPACE)
@OslcResourceShape(
        title = "OSLC Service Provider Resource Shape",
        describes = OslcConstants.TYPE_SERVICE_PROVIDER)
public class ServiceProvider extends AbstractResource {
    private final SortedSet<URI> details = new TreeSet<>();
    private final List<PrefixDefinition> prefixDefinitions = new ArrayList<>();
    private final List<Service> services = new ArrayList<>();

    private Date created; // TODO - ServiceProvider.created nice to have, but not required.
    private String description;
    private String identifier; // TODO - ServiceProvider.identifier nice to have, but not required.
    private OAuthConfiguration oauthConfiguration;
    private Publisher publisher;
    private String title;

    public ServiceProvider() {
        super();
    }

    public void addService(final Service srvc) {
        this.services.add(srvc);
    }

    @OslcDescription("The date and time that this resource was created")
    @OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "created")
    @OslcReadOnly
    @OslcTitle("Created")
    public final Date getCreated() {
        return created;
    }

    @OslcDescription("Description of the service provider")
    @OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "description")
    @OslcReadOnly
    @OslcTitle("Description")
    @OslcValueType(ValueType.XMLLiteral)
    public String getDescription() {
        return description;
    }

    @OslcDescription(
            "URLs that may be used to retrieve web pages to determine additional details about the"
                    + " service provider")
    @OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "details")
    @OslcReadOnly
    @OslcTitle("Details")
    public URI[] getDetails() {
        return details.toArray(new URI[details.size()]);
    }

    @OslcDescription("A unique identifier for this resource")
    @OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "identifier")
    @OslcReadOnly
    @OslcTitle("Identifier")
    public final String getIdentifier() {
        return identifier;
    }

    @OslcDescription(
            "Defines the three OAuth URIs required for a client to act as an OAuth consumer")
    @OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "oauthConfiguration")
    @OslcRange(OslcConstants.TYPE_O_AUTH_CONFIGURATION)
    @OslcReadOnly
    @OslcRepresentation(Representation.Inline)
    @OslcTitle("OAuth Configuration")
    @OslcValueShape(
            OslcConstants.PATH_RESOURCE_SHAPES + "/" + OslcConstants.PATH_OAUTH_CONFIGURATION)
    @OslcValueType(ValueType.LocalResource)
    public OAuthConfiguration getOauthConfiguration() {
        return oauthConfiguration;
    }

    @OslcDescription(
            "Defines namespace prefixes for use in JSON representations and in forming OSLC Query"
                    + " Syntax strings")
    @OslcName("prefixDefinition")
    @OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "prefixDefinition")
    @OslcRange(OslcConstants.TYPE_PREFIX_DEFINITION)
    @OslcReadOnly
    @OslcRepresentation(Representation.Inline)
    @OslcTitle("Prefix Definitions")
    @OslcValueShape(OslcConstants.PATH_RESOURCE_SHAPES + "/" + OslcConstants.PATH_PREFIX_DEFINITION)
    @OslcValueType(ValueType.LocalResource)
    public PrefixDefinition[] getPrefixDefinitions() {
        return prefixDefinitions.toArray(new PrefixDefinition[prefixDefinitions.size()]);
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

    @OslcDescription("Describes services offered by the service provider")
    @OslcName("service")
    @OslcOccurs(Occurs.OneOrMany)
    @OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "service")
    @OslcRange(OslcConstants.TYPE_SERVICE)
    @OslcReadOnly
    @OslcRepresentation(Representation.Inline)
    @OslcTitle("Services")
    @OslcValueShape(OslcConstants.PATH_RESOURCE_SHAPES + "/" + OslcConstants.PATH_SERVICE)
    @OslcValueType(ValueType.LocalResource)
    public Service[] getServices() {
        return services.toArray(new Service[services.size()]);
    }

    @OslcDescription("Title of the service provider")
    @OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "title")
    @OslcReadOnly
    @OslcTitle("Title")
    @OslcValueType(ValueType.XMLLiteral)
    public String getTitle() {
        return title;
    }

    public final void setCreated(final Date created) {
        this.created = created;
    }

    public void setDescription(final String description) {
        this.description = description;
    }

    public void setDetails(final URI[] details) {
        this.details.clear();
        if (details != null) {
            this.details.addAll(Arrays.asList(details));
        }
    }

    public final void setIdentifier(final String identifier) {
        this.identifier = identifier;
    }

    public void setOauthConfiguration(final OAuthConfiguration oauthConfiguration) {
        this.oauthConfiguration = oauthConfiguration;
    }

    public void setPrefixDefinitions(final PrefixDefinition[] prefixDefinitions) {
        this.prefixDefinitions.clear();
        if (prefixDefinitions != null) {
            this.prefixDefinitions.addAll(Arrays.asList(prefixDefinitions));
        }
    }

    public void setPublisher(final Publisher publisher) {
        this.publisher = publisher;
    }

    public void setServices(final Service[] services) {
        this.services.clear();
        if (services != null) {
            this.services.addAll(Arrays.asList(services));
        }
    }

    public void setTitle(final String title) {
        this.title = title;
    }
}
