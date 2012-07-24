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

import org.antlr.runtime.RecognitionException;

/**
 * Wrapper for {@link RecognitionException} so clients of OLSC query
 * functionality don't need to import ANTLR classes.
 */
public class ParseException extends Exception
{
    ParseException(RecognitionException cause)
    {
        super(cause);
    }
    
    ParseException(String msg)
    {
        super(msg);
    }
    
    private static final long serialVersionUID = 2373494371127406191L;
}
