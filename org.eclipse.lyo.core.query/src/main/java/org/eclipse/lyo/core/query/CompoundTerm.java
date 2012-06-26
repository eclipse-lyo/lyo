/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
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
 *    Steve Pitschke - initial API and implementation
 *******************************************************************************/

package org.eclipse.lyo.core.query;

import java.util.List;

/**
 * Compound term from oslc.where clause.<p>
 * 
 * <b>Note:</b> If top-level compound term, {@link SimpleTerm#property()}
 * will return <code>null</code>.
 */
public interface CompoundTerm extends SimpleTerm
{
    /**
     * @return immutable list of child simple terms
     */
    List<SimpleTerm> children();
}
