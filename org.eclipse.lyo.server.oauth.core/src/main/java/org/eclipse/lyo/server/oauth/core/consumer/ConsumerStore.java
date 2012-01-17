/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
 *
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the Eclipse Public License v1.0
 *  and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *  
 *  The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 *  and the Eclipse Distribution License is available at
 *  http://www.eclipse.org/org/documents/edl-v10.php.
 *  
 *  Contributors:
 *  
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.server.oauth.core.consumer;

import java.io.IOException;
import java.util.Collection;

/**
 * Manages persistence of OAuth consumers.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 */
public interface ConsumerStore {
	public Collection<LyoOAuthConsumer> load() throws IOException;
}
