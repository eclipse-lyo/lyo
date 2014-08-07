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
 *******************************************************************************/
package org.eclipse.lyo.core.trs;

import org.eclipse.lyo.oslc4j.core.model.AbstractResource;

/**
 * Abstract base class for {@link ChangeLog} and {@link EmptyChangeLog}
 * due to differences between RDF/XML output for a change log with
 * and without entries.
 */
public abstract class AbstractChangeLog extends AbstractResource
{
}
