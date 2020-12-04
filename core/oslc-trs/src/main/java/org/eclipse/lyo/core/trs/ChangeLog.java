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
package org.eclipse.lyo.core.trs;

import static org.eclipse.lyo.core.trs.TRSConstants.TRS_CHANGE;
import static org.eclipse.lyo.core.trs.TRSConstants.TRS_NAMESPACE;
import static org.eclipse.lyo.core.trs.TRSConstants.TRS_PREVIOUS;
import static org.eclipse.lyo.core.trs.TRSConstants.TRS_TERM_CHANGE;
import static org.eclipse.lyo.core.trs.TRSConstants.TRS_TERM_PREVIOUS;
import static org.eclipse.lyo.core.trs.TRSConstants.TRS_TYPE_CHANGE_LOG;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;

/**
 * A Change Log provides a list of changes. The following example illustrates
 * the contents of a Change Log:
 *
 * <pre>
 * # Resource: http://cm1.example.com/trackedResourceSet
 * {@literal @prefix trs: <http://open-services.net/ns/core/trs#> .}
 * {@literal @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .}
 *
 * {@code
 * <http://cm1.example.com/trackedResourceSet>
 * 	 a trs:TrackedResourceSet ;
 * 	 trs:base <http://cm1.example.com/baseResources> ;
 * 	 trs:changeLog [
 * 	   a trs:ChangeLog ;
 * 	   trs:change <urn:urn-3:cm1.example.com:2010-10-27T17:39:33.000Z:103> ;
 * 	   trs:change <urn:urn-3:cm1.example.com:2010-10-27T17:39:32.000Z:102> ;
 * 	   trs:change <urn:urn-3:cm1.example.com:2010-10-27T17:39:31.000Z:101> .
 * 	 ] .
 *
 * <urn:urn-3:cm1.example.com:2010-10-27T17:39:33.000Z:103>
 * 	 a trs:Creation ;
 * 	 trs:changed <http://cm1.example.com/bugs/23> ;
 * 	 trs:order "103"^^xsd:integer .
 *
 * <urn:urn-3:cm1.example.com:2010-10-27T17:39:32.000Z:102>
 * 	 a trs:Modification ;
 * 	 trs:changed <http://cm1.example.com/bugs/22> ;
 * 	 trs:order "102"^^xsd:integer .
 *
 * <urn:urn-3:cm1.example.com:2010-10-27T17:39:31.000Z:101>
 * 	 a trs:Deletion ;
 * 	 trs:changed <http://cm1.example.com/bugs/21> ;
 * 	 trs:order "101"^^xsd:integer .
 * }
 * </pre>
 *
 * <p>
 * As shown, a Change Log provides a set of Change Event entries in a
 * multi-valued RDF property called trs:change.
 *
 * <p>
 * Change Events MUST have URIs (i.e., they cannot be Blank Nodes) to allow
 * Clients to recognize entries they have seen before. The URI is only used to
 * identify an event (i.e., it need not be HTTP GETable).
 *
 * <p>
 * Each Change Event has a sequence number, trs:order; sequence numbers are
 * non-negative integer values that increase over time. A Change Event entry
 * carries the URI of the changed Resource, trs:changed, and an indication, via
 * rdf:type (a.k.a. "a" in Turtle), of whether the Resource was added to the
 * Resource Set, removed from the Resource Set, or changed state while a member
 * of the Resource Set. The entry with the highest trs:order value (i.e., 103 in
 * this example) is the most recent change. As changes continue to occur, a
 * Server MUST add new Change Events to the newest Change Log segment. The
 * sequence number (i.e., trs:order) of newer entries MUST be greater than
 * previous ones. The sequence numbers MAY be consecutive numbers but need not
 * be.
 *
 * <p>
 * Note that the actual time of change is not included in a Change Event. Only a
 * sequence number, representing the "sequence in time" of each change is
 * provided. The URI of a Change Event MUST be guaranteed unique, even in the
 * wake of a Server roll back where sequence numbers get reused. A time stamp
 * MAY be used to generate such a URI, as in the above example, although other
 * ways of generating a unique URI are also possible.
 *
 * <p>
 * A Change Log represents a series of changes to its corresponding Resource Set
 * over some period of time. The Change Log MUST contain Change Events for every
 * Resource creation, deletion, and modification during that period. A Server
 * MUST report a Resource modification event if a GET on it would return a
 * semantically different response from previously. For a resource with RDF
 * content, a modification is anything that would affect the set of RDF triples
 * in a significant way. A Server MAY safely report a modification event even in
 * cases where there would be no significant difference in response. Some cases
 * of modifications that would be considered semantically different from
 * previous or significant difference would be: inserted triple, removed triple,
 * triple replaced (new object/literal, e.g. changing boolean literal "true" to
 * "false"), replaced vocabulary term used (e.g. change from dcterms:title to
 * rdfs:label).
 *
 * <p>
 * The Server SHOULD NOT report unnecessary Change Events although it might
 * happen, for example, if changes occur while the base is being computed. A
 * Client SHOULD ignore a creation event for a Resource that is already a member
 * of the Resource Set, and SHOULD ignore a deletion or modification event for a
 * Resource that is not a member of the Resource Set.
 */
@OslcNamespace(TRS_NAMESPACE)
@OslcResourceShape(title = "Change Log	Shape", describes = TRS_TYPE_CHANGE_LOG)
public class ChangeLog extends AbstractChangeLog {
    private List<ChangeEvent> change = new ArrayList<>();
    private URI previous;

    /**
     * @return the change
     */
    @OslcName(TRS_TERM_CHANGE)
    @OslcDescription("URI references to the list of Change Event entries.")
    @OslcPropertyDefinition(TRS_CHANGE)
    @OslcTitle("Change")
    public List<ChangeEvent> getChange() {
        return change;
    }

    /**
     * @param change the changes to set
     */
    public void setChange(List<ChangeEvent> change) {
        if (change == null) {
            throw new IllegalArgumentException("Change Event list must not be null");
        }
        this.change = change;
    }

    /**
     * @return the previous
     */
    @OslcName(TRS_TERM_PREVIOUS)
    @OslcDescription(
            "The continuation of the Change Log, containing the next group of chronologically " +
                    "earlier Change Events. ")
    @OslcPropertyDefinition(TRS_PREVIOUS)
    @OslcTitle("Previous")
    public URI getPrevious() {
        return previous;
    }

    /**
     * @param previous the previous to set
     */
    public void setPrevious(URI previous) {
        this.previous = previous;
    }
}
