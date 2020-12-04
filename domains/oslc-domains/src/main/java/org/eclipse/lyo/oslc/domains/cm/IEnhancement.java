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

package org.eclipse.lyo.oslc.domains.cm;

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

import org.eclipse.lyo.oslc.domains.cm.Oslc_cmDomainConstants;

import org.eclipse.lyo.oslc.domains.cm.IDefect;
import org.eclipse.lyo.oslc.domains.rm.IRequirement;
import org.eclipse.lyo.oslc.domains.IAgent;
import org.eclipse.lyo.oslc.domains.IPerson;
import org.eclipse.lyo.oslc.domains.IPerson;
import org.eclipse.lyo.oslc4j.core.model.IDiscussion;
import org.eclipse.lyo.oslc.domains.rm.IRequirement;
import org.eclipse.lyo.oslc.domains.cm.IChangeRequest;
import org.eclipse.lyo.oslc.domains.cm.IPriority;
import org.eclipse.lyo.oslc.domains.cm.IState;
import org.eclipse.lyo.oslc.domains.config.IChangeSet;
import org.eclipse.lyo.oslc.domains.rm.IRequirement;

// Start of user code imports
// End of user code

@OslcNamespace(Oslc_cmDomainConstants.ENHANCEMENT_NAMESPACE)
@OslcName(Oslc_cmDomainConstants.ENHANCEMENT_LOCALNAME)
@OslcResourceShape(title = "Enhancement Resource Shape", describes = Oslc_cmDomainConstants.ENHANCEMENT_TYPE)
public interface IEnhancement
{



}

