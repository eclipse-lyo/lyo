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
 *     Paul McMahan <pmcmahan@us.ibm.com>     - initial implementation
 *******************************************************************************/
package org.eclipse.lyo.client.oslc.samples.automation;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URI;
import java.util.Properties;

import org.apache.commons.io.FileUtils;

/**
 * Subclass of Properties class that writes through to a backing file when a
 * property is set.
 */
public class WriteThroughProperties extends Properties {

	private static final long serialVersionUID = 1L;

	private final File adapterPropertiesFile;

	/**
	 * Default constructor.
	 *
	 * @param fileUri
	 *            URI for a local File containing the properties
	 * @throws IOException
	 */
	public WriteThroughProperties(URI fileUri) throws IOException {

		this.adapterPropertiesFile = new File(fileUri);

		FileInputStream fis = FileUtils.openInputStream(adapterPropertiesFile);

		load(fis);

		fis.close();

	}

	@Override
	public synchronized Object setProperty(String key, String value) {

		Object property = super.setProperty(key, value);

		try {

			FileOutputStream fos = FileUtils.openOutputStream(adapterPropertiesFile);

			store(fos, "Write through save for property : " + key);

			fos.close();

		} catch (IOException e) {

			throw new RuntimeException(e);

		}

		return property;
	}

}
