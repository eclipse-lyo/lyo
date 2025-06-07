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
import java.util.HashSet;
import java.util.Set;
import org.eclipse.lyo.oslc.domains.RdfsDomainConstants;
import org.eclipse.lyo.oslc4j.core.OSLC4JUtils;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRange;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.core.model.Occurs;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.ResourceShape;
import org.eclipse.lyo.oslc4j.core.model.ResourceShapeFactory;
import org.eclipse.lyo.oslc4j.core.model.ValueType;

// Start of user code imports
// End of user code

// Start of user code preClassCode
// End of user code

// Start of user code classAnnotations
// End of user code
@OslcNamespace(Oslc_configDomainConstants.CHANGESET_NAMESPACE)
@OslcName(Oslc_configDomainConstants.CHANGESET_LOCALNAME)
@OslcResourceShape(title = "ChangeSet Shape", describes = Oslc_configDomainConstants.CHANGESET_TYPE)
public class ChangeSet extends Configuration implements IChangeSet {
    // Start of user code attributeAnnotation:accepts
    // End of user code
    private Set<Link> accepts = new HashSet<>();
    // Start of user code attributeAnnotation:overrides
    // End of user code
    private Link overrides;

    // Start of user code classAttributes
    // End of user code
    // Start of user code classMethods
    // End of user code
    public ChangeSet() {
        super();

        // Start of user code constructor1
        // End of user code
    }

    public ChangeSet(final URI about) {
        super(about);

        // Start of user code constructor2
        // End of user code
    }

    public static ResourceShape createResourceShape()
            throws OslcCoreApplicationException, URISyntaxException {
        return ResourceShapeFactory.createResourceShape(
                OSLC4JUtils.getServletURI(),
                OslcConstants.PATH_RESOURCE_SHAPES,
                Oslc_configDomainConstants.CHANGESET_PATH,
                ChangeSet.class);
    }

    public String toString() {
        return toString(false);
    }

    public String toString(boolean asLocalResource) {
        String result = "";
        // Start of user code toString_init
        // End of user code

        if (asLocalResource) {
            result =
                    result
                            + "{a Local ChangeSet Resource} - update ChangeSet.toString() to"
                            + " present resource as desired.";
            // Start of user code toString_bodyForLocalResource
            // End of user code
        } else {
            result = String.valueOf(getAbout());
        }

        // Start of user code toString_finalize
        // End of user code

        return result;
    }

    public void addAccepts(final Link accepts) {
        this.accepts.add(accepts);
    }

    // Start of user code getterAnnotation:accepts
    // End of user code
    @OslcName("accepts")
    @OslcPropertyDefinition(
            Oslc_configDomainConstants.CONFIGURATION_MANAGEMENT_NAMSPACE + "accepts")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRange({RdfsDomainConstants.CLASS_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getAccepts() {
        // Start of user code getterInit:accepts
        // End of user code
        return accepts;
    }

    // Start of user code getterAnnotation:overrides
    // End of user code
    @OslcName("overrides")
    @OslcPropertyDefinition(
            Oslc_configDomainConstants.CONFIGURATION_MANAGEMENT_NAMSPACE + "overrides")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.Resource)
    @OslcRange({Oslc_configDomainConstants.CONFIGURATION_TYPE})
    @OslcReadOnly(false)
    public Link getOverrides() {
        // Start of user code getterInit:overrides
        // End of user code
        return overrides;
    }

    // Start of user code setterAnnotation:accepts
    // End of user code
    public void setAccepts(final Set<Link> accepts) {
        // Start of user code setterInit:accepts
        // End of user code
        this.accepts.clear();
        if (accepts != null) {
            this.accepts.addAll(accepts);
        }
        // Start of user code setterFinalize:accepts
        // End of user code
    }

    // Start of user code setterAnnotation:overrides
    // End of user code
    public void setOverrides(final Link overrides) {
        // Start of user code setterInit:overrides
        // End of user code
        this.overrides = overrides;
        // Start of user code setterFinalize:overrides
        // End of user code
    }
}
