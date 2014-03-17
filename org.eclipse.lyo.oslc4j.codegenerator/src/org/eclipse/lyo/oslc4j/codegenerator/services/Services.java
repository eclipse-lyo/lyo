/*******************************************************************************
Copyright (c) 2013 Jad El-khoury

All rights reserved. This program and the accompanying materials
are made available under the terms of the Eclipse Public License v1.0
and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 
The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
and the Eclipse Distribution License is available at
http://www.eclipse.org/org/documents/edl-v10.php.

Contributors:

    Jad El-khoury          - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.codegenerator.services;

public class Services {

	public String OBS_attributeTypeDeclaration(String valueType, String occurs, String range) {
	String type = "";
	switch (valueType) {
		case ("Boolean"):
		case ("String"):
			type = valueType;
		break;
		case ("DateTime"):
			type = "Date";
		break;
		case ("URI"):
			type = "java.net.URI";
		break;
		case ("Resource"):
			type = range;
		break;	
	}
	
	if (occurs.equals("zeroOrMany") || (occurs.equals("oneOrMany"))) {
		type = "java.util.ArrayList<" + type + ">";
	}

	return type;
  }

}