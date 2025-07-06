// Start of user code Copyright
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
// End of user code

package org.eclipse.lyo.oslc.domains.config;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.text.SimpleDateFormat;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.Iterator;

import org.eclipse.lyo.oslc4j.core.annotation.OslcAllowedValue;
import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcMemberProperty;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRdfCollectionType;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRange;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRepresentation;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.core.model.Occurs;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.Representation;
import org.eclipse.lyo.oslc4j.core.model.ValueType;

import org.eclipse.lyo.oslc.domains.config.Oslc_configDomainConstants;
import org.eclipse.lyo.oslc.domains.config.Oslc_configDomainConstants;
import org.eclipse.lyo.oslc.domains.RdfsDomainConstants;
import org.eclipse.lyo.oslc.domains.IAgent;
import org.eclipse.lyo.oslc.domains.IRdfsClass;
import org.eclipse.lyo.oslc.domains.config.IComponent;
import org.eclipse.lyo.oslc.domains.config.IConfiguration;
import org.eclipse.lyo.oslc.domains.config.IContribution;
import org.eclipse.lyo.oslc.domains.IPerson;
import org.eclipse.lyo.oslc.domains.config.ISelections;
// spotless:off
// Start of user code imports
// End of user code
// spotless:on

@OslcNamespace(Oslc_configDomainConstants.CHANGESET_NAMESPACE)
@OslcName(Oslc_configDomainConstants.CHANGESET_LOCALNAME)
@OslcResourceShape(title = "ChangeSet Shape", describes = Oslc_configDomainConstants.CHANGESET_TYPE)
public interface IChangeSet
{

    public void addAccepts(final Link accepts );

    @OslcName("accepts")
    @OslcPropertyDefinition(Oslc_configDomainConstants.CONFIGURATION_MANAGEMENT_NAMSPACE + "accepts")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRange({RdfsDomainConstants.CLASS_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getAccepts();

    @OslcName("overrides")
    @OslcPropertyDefinition(Oslc_configDomainConstants.CONFIGURATION_MANAGEMENT_NAMSPACE + "overrides")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.Resource)
    @OslcRange({Oslc_configDomainConstants.CONFIGURATION_TYPE})
    @OslcReadOnly(false)
    public Link getOverrides();


    public void setAccepts(final Set<Link> accepts );
    public void setOverrides(final Link overrides );
}

