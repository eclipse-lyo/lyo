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
 *	  Kevin Bauer - Initial implementation
 *	  David Terry - 2.0 implementation
 *******************************************************************************/
package org.eclipse.lyo.core.trs;

import static org.eclipse.lyo.core.trs.TRSConstants.TRS_NAMESPACE;
import static org.eclipse.lyo.core.trs.TRSConstants.TRS_TYPE_CHANGE_LOG;

import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;

/**
 * A Change Log provides a list of changes organized in inverse chronological 
 * order, most recent first. The following example illustrates the contents of a 
 * Change Log:
 * <pre>
# Resource: http://cm1.example.com/trackedResourceSet
{@literal @prefix trs: <http://open-services.net/ns/core/trs#> .}
{@literal @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .}

{@code
<http://cm1.example.com/trackedResourceSet>
  a trs:TrackedResourceSet ;
  trs:base <http://cm1.example.com/baseResources> ;
  trs:changeLog
			 [ a	   trs:ChangeLog
			 ] .
}
</pre>

An empty change log essentially contains nothing. This class exists to allow
JAX-RS implementations a way to generate the empty change log in the TRS 
resource's output. 
 */
@OslcNamespace(TRS_NAMESPACE)
@OslcResourceShape(title = "Change Log	Shape", describes = TRS_TYPE_CHANGE_LOG)
@OslcName("ChangeLog")
public class EmptyChangeLog extends AbstractChangeLog
{
	
}
