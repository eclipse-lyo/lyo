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
 *     Keith Wells             - initial API and implementation
 *     Sam Padgett           - initial API and Implementation
 *     Jim Conallen           - initial API and implementation
 *
 *******************************************************************************/
package org.eclipse.lyo.samples.sharepoint.util;

import java.io.StringWriter;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;

public class XmlUtils {
	
	@SuppressWarnings("nls")
	public static String encode(String exp) {
		if( exp != null ) {
			String encoded = exp.replaceAll("&", "&amp;"); 
			encoded = encoded.replaceAll(">", "&gt;");  
			encoded = encoded.replaceAll("<", "&lt;"); 
			encoded = encoded.replaceAll("\"", "&quot;"); 
			encoded = encoded.replaceAll("'", "&apos;"); 
			return encoded;
		}
		return "";
	}
	
	public static String prettyPrint( Document doc ) {
        TransformerFactory tfactory = TransformerFactory.newInstance();
        Transformer serializer;
        StringWriter writer = new StringWriter();
        try {
            serializer = tfactory.newTransformer();
            //Setup indenting to "pretty print"
            serializer.setOutputProperty(OutputKeys.INDENT, "yes"); //$NON-NLS-1$
            serializer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2"); //$NON-NLS-1$ //$NON-NLS-2$
            
            serializer.transform(new DOMSource(doc), new StreamResult(writer));
            return writer.toString();
        } catch (TransformerException e) {
            throw new RuntimeException(e);
        }
	}
	
}
