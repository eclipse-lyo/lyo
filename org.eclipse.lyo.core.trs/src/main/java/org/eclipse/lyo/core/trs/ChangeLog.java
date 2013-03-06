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
 * Contributors:
 * 
 *    Kevin Bauer - Initial implementation 
 *******************************************************************************/
package org.eclipse.lyo.core.trs;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRdfCollectionType;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;

import static org.eclipse.lyo.core.trs.TRSConstants.*;

/**
 * A Change Log provides a list of changes organized in inverse chronological 
 * order, most recent first. The following example illustrates the contents of 
 * a Change Log:
 *  
# Resource: http://cm1.example.com/trackedResourceSet
@prefix trs: <http://jazz.net/ns/trs#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

<http://cm1.example.com/trackedResourceSet>
  a trs:TrackedResourceSet ;
  trs:base <http://cm1.example.com/baseResources> ;
  trs:changeLog [
    a trs:ChangeLog ;
    trs:changes (
      <urn:urn-3:cm1.example.com:2010-10-27T17:39:33.000Z:103> 
      <urn:urn-3:cm1.example.com:2010-10-27T17:39:32.000Z:102>
      <urn:urn-3:cm1.example.com:2010-10-27T17:39:31.000Z:101>
    ) .
  ] .

<urn:urn-3:cm1.example.com:2010-10-27T17:39:33.000Z:103> 
  a trs:Creation ;
  trs:changed <http://cm1.example.com/bugs/23> ;
  trs:order "103"^^xsd:integer .

<urn:urn-3:cm1.example.com:2010-10-27T17:39:32.000Z:102>
  a trs:Modification ;
  trs:changed <http://cm1.example.com/bugs/22> ;
  trs:order "102"^^xsd:integer .

<urn:urn-3:cm1.example.com:2010-10-27T17:39:31.000Z:101>
  a trs:Deletion ;
  trs:changed <http://cm1.example.com/bugs/21> ;
  trs:order "101"^^xsd:integer .
  
As shown, a Change Log provides a set of Change Event entries in a single-valued 
RDF collection-type property called trs:changes. An RDF collection, i.e., a 
linked list (reference: RDF Collections), is used in the Change Log to ensure 
that the entries retain their correct (inverse chronological) order. 

Change Events MUST have URIs (i.e., they cannot be Blank Nodes) to allow Clients
to recognize entries they have seen before. The URI is only used to identify an 
event (i.e., it need not be HTTP GETable) and therefore MAY be a URN, as shown 
in the example.

Each Change Event has a sequence number, trs:order; sequence numbers are 
non-negative integer values that increase over time. A Change Event entry 
carries the URI of the changed Resource, trs:changed, and an indication, 
via rdf:type (a.k.a. "a" in Turtle), of whether the Resource was added to the 
Resource Set, removed from the Resource Set, or changed state while a member of 
the Resource Set. The first entry in the Change Log, i.e., "103" in this 
example, is the most recent change. As changes continue to occur, a Server MUST
add new Change Events to the front of the list. The sequence number 
(i.e., trs:order) of newer entries MUST be greater than previous ones. The 
sequence numbers MAY be consecutive numbers but need not be. 

Note that the actual time of change is not included in a Change Event. Only a 
sequence number, representing the "sequence in time" of each change is provided. 
The URI of a Change Event MUST be guaranteed unique, even in the wake of a 
Server roll back where sequence numbers get reused. A time stamp MAY be used to 
generate such a URI, as in the above example, although other ways of generating 
a unique URI are also possible. 

A Change Log represents a series of changes to its corresponding Resource Set 
over some period of time. The Change Log MUST contain Change Events for every 
Resource creation, deletion, and modification during that period. A Server MUST
report a Resource modification event if a GET on it would return a semantically 
different response from previously. For a resource with RDF content, a 
modification is anything that would affect the set of RDF triples in a 
significant way. A Server MAY safely report a modification event even in cases 
where there would be no significant difference in response.
 
The Server SHOULD NOT report unnecessary Change Events although it might happen,
for example, if changes occur while the base is being computed. A Client SHOULD 
ignore a creation event for a Resource that is already a member of the Resource 
Set, and SHOULD ignore a deletion or modification event for a Resource that is 
not a member of the Resource Set. 
 */
@OslcNamespace(TRS_NAMESPACE)
@OslcResourceShape(title = "Change Log  Shape", describes = TRS_TYPE_CHANGE_LOG)
public class ChangeLog extends AbstractChangeLog
{
	private List<ChangeEvent> changes;
	private URI previous;
	
	/**
	 * @return the changes
	 */
	@OslcName(TRS_TERM_CHANGES)
	@OslcDescription("The list of Change Event entries, ordered by decreasing Change Event trs:order. Events that occurred later appear earlier in the list.")
	@OslcPropertyDefinition(TRS_CHANGES)
	@OslcTitle("Changes")
    @OslcRdfCollectionType
	public List<ChangeEvent> getChanges() {
		if(changes == null){
			changes = new ArrayList<ChangeEvent>();
		}
		return changes;
	}
	
	/**
	 * @param changes the changes to set
	 */
	public void setChanges(List<ChangeEvent> changes) {
		this.changes = changes;
	}
	
	/**
	 * @return the previous
	 */
	@OslcName(TRS_TERM_PREVIOUS)
	@OslcDescription("The continuation of the Change Log, containing the next group of chronologically earlier Change Events. ")
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
