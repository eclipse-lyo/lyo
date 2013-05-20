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

import static org.eclipse.lyo.core.trs.TRSConstants.RDF_NIL;
import static org.eclipse.lyo.core.trs.TRSConstants.TRS_CHANGES;
import static org.eclipse.lyo.core.trs.TRSConstants.TRS_NAMESPACE;
import static org.eclipse.lyo.core.trs.TRSConstants.TRS_TERM_CHANGES;
import static org.eclipse.lyo.core.trs.TRSConstants.TRS_TYPE_CHANGE_LOG;

import java.net.URI;
import java.net.URISyntaxException;

import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;

/**
 * A Change Log provides a list of changes organized in inverse chronological 
 * order, most recent first. The following example illustrates the contents of a 
 * Change Log:
 * <pre>
# Resource: http://cm1.example.com/trackedResourceSet
{@literal @prefix trs: <http://jazz.net/ns/trs#> .}
{@literal @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .}

{@code
<http://cm1.example.com/trackedResourceSet>
  a trs:TrackedResourceSet ;
  trs:base <http://cm1.example.com/baseResources> ;
  trs:changeLog [
    a trs:ChangeLog ;
    trs:changes <http://www.w3.org/2000/01/rdf-schema#nil> .
  ] .
}
</pre>

As shown, an empty Change Log just provides an single trs;changes element with a value
of "http://www.w3.org/2000/01/rdf-schema#nil".
 */
@OslcNamespace(TRS_NAMESPACE)
@OslcResourceShape(title = "Change Log  Shape", describes = TRS_TYPE_CHANGE_LOG)
@OslcName("ChangeLog")
public class EmptyChangeLog extends AbstractChangeLog
{
	private URI changes;
	
	/**
	 * @return the changes
	 * @throws URISyntaxException 
	 */
	@OslcName(TRS_TERM_CHANGES)
	@OslcDescription("The list of Change Event entries, ordered by decreasing Change Event trs:order. Events that occurred later appear earlier in the list.")
	@OslcPropertyDefinition(TRS_CHANGES)
	@OslcTitle("Changes")
	public URI getChanges() throws URISyntaxException {
		if(changes == null) {
			return new URI(RDF_NIL);
		}
		
		return changes;
	}
	
	/**
	 * @param changes the changes to set
	 */
	public void setChanges(URI changes) {
		this.changes = changes;
	}
}
