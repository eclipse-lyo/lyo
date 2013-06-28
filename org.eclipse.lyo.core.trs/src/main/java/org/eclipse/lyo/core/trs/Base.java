/*******************************************************************************
 * Copyright (c) 2013 IBM Corporation.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 * 
 * * Contributors:
 * 
 *    Kevin Bauer - Initial implementation
 *    David Terry - TRS 2.0 compliant implementation
 *******************************************************************************/
package org.eclipse.lyo.core.trs;

import static org.eclipse.lyo.core.trs.TRSConstants.LDP_AGGREGATE_CONTAINER;
import static org.eclipse.lyo.core.trs.TRSConstants.LDP_NAMESPACE;
import static org.eclipse.lyo.core.trs.TRSConstants.LDP_TERM_AGGREGATE_CONTAINER;
import static org.eclipse.lyo.core.trs.TRSConstants.RDFS_MEMBER;
import static org.eclipse.lyo.core.trs.TRSConstants.RDFS_TERM_MEMBER;
import static org.eclipse.lyo.core.trs.TRSConstants.TRS_CUTOFFEVENT;
import static org.eclipse.lyo.core.trs.TRSConstants.TRS_TERM_CUTOFFEVENT;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcHidden;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;

/**
 * The Base of a Tracked Resource Set is a W3C Linked Data Platform (LDP)
 * Container where each member references a Resource that was in the Resource
 * Set at the time the Base was computed. HTTP GET on a Base URI returns an RDF
 * container with the following structure:
 * 
 * <pre>
 * {@literal @prefix trs: <http://open-services.net/ns/core/trs#> .}
 * {@literal @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .}
 * {@literal @prefix ldp: <http://www.w3.org/ns/ldp#> .}
 * 
 * {@code
 * <https://.../baseResources>
 *   a ldp:AggregateContainer;
 *   trs:cutoffEvent <#1> ; 
 *   rdfs:member <https://.../WorkItem/1> ;
 *   rdfs:member <https://.../WorkItem/2> ;
 *   rdfs:member <https://.../WorkItem/3> ;
 *   ...
 *   rdfs:member <https://.../WorkItem/199> ;
 *   rdfs:member <https://.../WorkItem/200> .
 * }
 * </pre>
 * 
 * <p>
 * Each Resource in the Resource Set MUST be referenced from the container using
 * an rdfs:member predicate. The Base MAY be broken into multiple pages in which
 * case the Server will respond with a 30x redirect message, directing the
 * Client to the first “page resource”. The representation of a page resource
 * will contain a subset of the Base’s rdfs:member predicates. In addition, it
 * will contain another triple, whose subject is the page resource itself (i.e.,
 * not the Base resource), with a reference to the next page:
 * 
 * <pre>
 * {@code
 * <https://.../baseResources/page1>
 *   a ldp:Page;
 *   ldp:pageOf <https://.../baseResource>;
 *   ldp:nextPage <https://../baseResources/page2> .
 * }
 * </pre>
 * 
 * <p>
 * The last page in the list is indicated by a ldp:nextPage value of rdf:nil.
 * The Tracked Resource Set protocol does not attach significance to the order
 * in which a Server enumerates the resources in the Base or breaks the Base up
 * into pages.
 * 
 * <p>
 * The first page of a Base MUST include a trs:cutoffEvent property, whose value
 * is the URI of the most recent Change Event in the corresponding Change Log
 * that is already reflected in the Base. This corresponds to the latest point
 * in the Change Log from which a Client can begin incremental
 * monitoring/updating if it wants to remain synchronized with further changes
 * to the Resource Set. As mentioned above, the cutoff Change Event MUST appear
 * in the non-truncated portion of the Change Log. When the trs:cutoffEvent is
 * rdf:nil, the Base enumerates the (possibly empty) Resource Set at the
 * beginning of time.
 * 
 * <p>
 * Because of the highly dynamic nature of the Resource Set, a Server may have
 * difficulty enumerating the exact set of resources at a point in time. Because
 * of that, the Base can be only an approximation of the Resource Set. A Base
 * might omit mention of a Resource that ought to have been included or include
 * a Resource that ought to have been omitted. For each erroneously reported
 * Resource in the Base, the Server MUST at some point include a corrective
 * Change Event in the Change Log more recent that the base cutoff event. The
 * corrective Change Event corrects the picture for that Resource, allowing the
 * Client to compute the correct set of member Resources. A corrective Change
 * Event might not appear in the Change Log that was retrieved when the Client
 * dereferenced the Tracked Resource Set URI. The Client might only see a
 * corrective Change Event when it processes the Change Log resource obtained by
 * dereferencing the Tracked Resource Set URI on later occasions.
 * 
 * <p>
 * When a Base is broken into pages, the Client will discover and retrieve Base
 * page resources to determine the Resources in the Base. A Client MUST retrieve
 * all the page resources of the Base. A Client MAY retrieve the Base page
 * resources in any order, including retrieving some Base page resources in
 * parallel. A Client retrieves the Base page resources at its own pace, and MAY
 * retrieve any of the Base page resources more than once. If the Server allows
 * the representation of Base page resources to vary over time, the Server MUST
 * ensure that the set of Resources a Client would infer as members is
 * necessarily an approximation of the Resource Set which, when corrected by
 * Change Events after the Base’s cutoff event, yields the correct set of member
 * Resources in the Resource Set.
 * 
 * <p>
 * A Server MUST refer to a given resource using the exact same URI in the Base
 * ( rdfs:member reference) and every Change Event ( trs:changed reference) for
 * that resource.
 */
@OslcNamespace(LDP_NAMESPACE)
@OslcName(LDP_TERM_AGGREGATE_CONTAINER)
@OslcResourceShape(title = "Tracked Resource Set Base Shape", describes = LDP_AGGREGATE_CONTAINER)
public class Base extends AbstractResource {
	private List<URI> members;
	private URI cutoffEvent;
	private Page nextPage;
	
	/**
	 * @return the members
	 */
	@OslcName(RDFS_TERM_MEMBER)
	@OslcDescription("A member Resource of the Resource Set.")
	@OslcPropertyDefinition(RDFS_MEMBER)
	@OslcTitle("Member")
	public List<URI> getMembers() {
		if(members == null){
			members = new ArrayList<URI>();
		}
		return members;
	}
	
	/**
	 * @param members the members to set
	 */
	public void setMembers(List<URI> members) {
		this.members = members;
	}

	/**
	 * @return the cutoffIdentifier
	 */
	@OslcName(TRS_TERM_CUTOFFEVENT)
	@OslcDescription("The most recent Change Log entry that is accounted for in this Base. When rdf:nil, the Base is an enumeration at the start of time.")
	@OslcPropertyDefinition(TRS_CUTOFFEVENT)
	@OslcTitle("Cutoff Event")
	public URI getCutoffEvent() {
		return cutoffEvent;
	}
	/**
	 * @param cutoffIdentifier the cutoffIdentifier to set
	 */
	public void setCutoffEvent(URI cutoffEvent) {
		this.cutoffEvent = cutoffEvent;
	}

	/**
	 * Return a Page object containing information about the next base page.
	 * The OslcHidden annotation works around a limitation in OSLC4J.  If we do
	 * not hide the nextPage variable we get an incorrect ldp:nextPage reference
	 * in the Base.
	 * 
	 * @return the nextPage
	 */
	@OslcHidden(value=true)
	public Page getNextPage() {
		return nextPage;
	}

	/**
	 * @param nextPage the nextPage to set
	 */
	public void setNextPage(Page nextPage) {
		this.nextPage = nextPage;
	}
}
