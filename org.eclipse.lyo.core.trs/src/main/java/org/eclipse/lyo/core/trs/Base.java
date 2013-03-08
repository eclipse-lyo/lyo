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
 *******************************************************************************/
package org.eclipse.lyo.core.trs;

import static org.eclipse.lyo.core.trs.TRSConstants.RDFS_MEMBER;
import static org.eclipse.lyo.core.trs.TRSConstants.RDFS_TERM_MEMBER;
import static org.eclipse.lyo.core.trs.TRSConstants.TRS_CUTOFFEVENT;
import static org.eclipse.lyo.core.trs.TRSConstants.TRS_NAMESPACE;
import static org.eclipse.lyo.core.trs.TRSConstants.TRS_NEXT_PAGE;
import static org.eclipse.lyo.core.trs.TRSConstants.TRS_TERM_CUTOFFEVENT;
import static org.eclipse.lyo.core.trs.TRSConstants.TRS_TERM_NEXT_PAGE;
import static org.eclipse.lyo.core.trs.TRSConstants.TRS_TYPE_BASE;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;

/**
 * The Base resources of a Tracked Resource Set are represented by an RDF 
 * container where each member references a Resource that was in the Resource 
 * Set at the time the Base was computed. HTTP GET on a Base URI returns an RDF
 * container with the following structure:
 * 
@prefix oslc_trs: <http://open-services.net/ns/core/trs#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

<https://.../myResources> 
  oslc_trs:cutoffIdentifier "2010-10-27T17:39:31.000Z#101" ;
  rdfs:member <https://.../WorkItem/1> ;
  rdfs:member <https://.../WorkItem/2> ;
  rdfs:member <https://.../WorkItem/3> ;
  ...
  rdfs:member <https://.../WorkItem/199> ;
  rdfs:member <https://.../WorkItem/200> .
  
Each Resource in the Resource Set MUST be referenced from the container using 
an rdfs:member predicate. The Base MAY be broken into multiple pages, in which 
case the standard OSLC paging (reference: Resource Paging) mechanism is used to 
connect one page to the next. (Note that an OSLC queryBase satisfies these 
requirements, which may allow a Server to use existing queryBases as Base 
containers, but there is no requirement that the Tracked Resource Set Base also 
be a queryBase.) The Tracked Resource Set protocol does not attach significance 
to the order in which a Server enumerates the resources in the Base or breaks 
the Base up into pages.

As shown above, a Base usually provides an oslc_trs:cutoffIdentifier property,
whose value is the identifier (i.e., dcterms:identifier) of the most recent 
Change Event in the corresponding Change Log that is already reflected in the 
Base. This corresponds to the latest point in the Change Log from which a 
Client can begin incremental monitoring/updating if it wants to remain 
synchronized with further changes to the Resource Set. As mentioned above, 
the cutoff Change Event MUST appear in the non-truncated portion of the Change 
Log. When the oslc_trs:cutoffIdentifier is omitted, the Base enumerates the 
(possibly empty) Resource Set at the beginning of time.

The Base is only an approximation of the Resource Set. A Base might omit mention
of a Resource that ought to have been included or include a Resource that ought
to have been omitted. For each erroneously reported Resource in the Base, the 
Server MUST at some point include a corrective Change Event in the Change Log 
more recent that the base cutoff event. The corrective Change Event corrects the
picture for that Resource, allowing the Client to compute the correct set of 
member Resources. A corrective Change Event might not appear in the Change Log 
that was retrieved when the Client dereferenced the Tracked Resource Set URI. 
The Client might only see a corrective Change Event when it processes the Change
Log resource obtained by dereferencing the Tracked Resource Set URI on later 
occasions.

A Server MUST refer to a given resource using the exact same URI in the Base 
( rdfs:member reference) and every Change Event ( oslc_trs:changed reference) 
for that resource.
 * 
 * @author kmbauer
 *
 */
@OslcNamespace(TRS_NAMESPACE)
@OslcResourceShape(title = "Tracked Resource Set Base  Shape", describes = TRS_TYPE_BASE)
public class Base extends AbstractResource {
	private List<URI> members;
	private URI cutoffEvent;
	private URI nextPage;
	
	
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
	 * @return the nextPage
	 */
	@OslcName(TRS_TERM_NEXT_PAGE)
	@OslcDescription("The representation of a page resource will contain a subset of the Base's rdfs:member predicates. In addition, it will contain another triple, whose subject is the page resource itself (i.e., not the Base resource), with a reference to the next page.")
	@OslcPropertyDefinition(TRS_NEXT_PAGE)
	@OslcTitle("Next Page")
	public URI getNextPage() {
		return nextPage;
	}

	/**
	 * @param nextPage the nextPage to set
	 */
	public void setNextPage(URI nextPage) {
		this.nextPage = nextPage;
	}
}
