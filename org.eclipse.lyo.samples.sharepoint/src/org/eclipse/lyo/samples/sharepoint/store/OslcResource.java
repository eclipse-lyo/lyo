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
package org.eclipse.lyo.samples.sharepoint.store;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Date;
import java.util.List;

import org.eclipse.lyo.samples.sharepoint.core.IConstants;
import org.eclipse.lyo.samples.sharepoint.store.ShareValue.ShareValueType;
import org.eclipse.lyo.samples.sharepoint.util.StringUtils;


/**
 * 
 *
 */
public class OslcResource extends ShareResource {

	/**
	 * @param uri
	 * @throws ShareServerException
	 */
	public OslcResource(String uri) throws ShareServerException {
		super(uri);
	}

	/**
	 * @param uri
	 * @param statements
	 */
	public OslcResource(String uri, List<ShareStatement> statements) {
		super(uri,statements);
	}

	/**
	 * @return
	 */
	public Date getCreated() {
		ShareValue prop = this.getFirstPropertyValue(IConstants.DCTERMS_CREATED);
		if (prop != null) {
			try{
				return prop.dateValue();
			}catch( IncompatibleValueException e) {
				// log it?
			}
		} else {
			// we need to create it
			try {
				Date now = new Date();
				this.setDateProperty(IConstants.DCTERMS_CREATED, now );
				return now;
			} catch (ShareServerException e) {
				// log it?
			}
		}
		return null;
	}

	/**
	 * @return user
	 */
	public String getCreator() {
		String user = null;
		ShareValue prop = this.getFirstPropertyValue(IConstants.DCTERMS_CREATOR);
		if (prop != null) {
			user = prop.stringValue();
		} else {
			try {
				// need to set it.
				user = ShareStore.getStore().getDefaultUserUri();
			} catch (ShareServerException e) {
				// log it?
			}
		}
		return user;
	}

	/**
	 * @return modified date or created date
	 */
	public Date getModified()  {
		ShareValue prop = this.getFirstPropertyValue(IConstants.DCTERMS_MODIFIED);
		if( prop != null ) {
			try{
				return prop.dateValue();
			} catch( IncompatibleValueException e ) {
				// log it?
			}
		} 
		return getCreated();
	}

	/**
	 * @return contributor
	 */
	public String getContributor() {
		ShareValue prop = this.getFirstPropertyValue(IConstants.DCTERMS_CONTRIBUTOR);
		if( prop != null ) {
			return prop.stringValue();
		}
		return null;
	}

	/**
	 * @return ETag
	 * @throws ShareServerException
	 */
	public String getETag() throws ShareServerException {
		// just MD5 hash of last modified date
		Date modified = getModified();
		String str = StringUtils.rfc2822(modified);
		try {
			MessageDigest md = MessageDigest.getInstance("MD5"); //$NON-NLS-1$
			md.update(str.getBytes());
			byte buf[] = md.digest();
			StringBuffer strBuf = new StringBuffer();
			for (int i = 0; i < buf.length; i++) {
				String hex = Integer.toHexString(0xff & buf[i]);
				if (hex.length() == 1)
					strBuf.append('0'); // prepend 0 to make sure all values are two chars
				strBuf.append(hex);
			}
			return strBuf.toString();
		} catch (NoSuchAlgorithmException e) {
			throw new ShareServerException(e);
		}
	}

	/**
	 * @return title
	 * @default [URI]
	 */
	public String getTitle() {
		ShareValue prop = this.getFirstPropertyValue(IConstants.DCTERMS_TITLE);
		if( prop != null ) {
			return prop.stringValue();
		} else {
			return '[' + this.getUri() + ']'; // default to uri
		}
	}

	/**
	 * @param title
	 * @throws ShareServerException
	 */
	public void setTitle(String title) throws ShareServerException {
		setStringProperty(IConstants.DCTERMS_TITLE, title);
	}

	/**
	 * @return identifier
	 */
	public String getIdentifier() {
		ShareValue prop = this.getFirstPropertyValue(IConstants.DCTERMS_IDENTIFIER);
		if( prop != null ){
			return prop.stringValue();
		}

		// we can derive this from the uri
		int pos = uri.lastIndexOf('/');
		if (pos > 0) {
			return uri.substring(pos + 1);
		}
		return null;

	}

	/**
	 * @param identifier
	 * @throws ShareServerException
	 */
	public void setIdentifier(String identifier) throws ShareServerException {
		this.setStringProperty(IConstants.DCTERMS_IDENTIFIER, identifier);
	}

	/**
	 * @param created
	 * @throws ShareServerException
	 */
	public void setCreated(Date created) throws ShareServerException {
		this.setDateProperty(IConstants.DCTERMS_CREATED, created);
	}

	/**
	 * @param userUri
	 * @throws ShareServerException
	 */
	public void setCreator(String userUri) throws ShareServerException {
		addProperty(IConstants.DCTERMS_CREATOR, ShareValueType.URI, userUri, true);
	}

	/**
	 * @param modified
	 * @throws ShareServerException
	 */
	public void setModified(Date modified) throws ShareServerException {
		this.setDateProperty(IConstants.DCTERMS_MODIFIED, modified);
	}

	/**
	 * @param userUri
	 * @throws ShareServerException
	 */
	public void setContributor(String userUri) throws ShareServerException {
		this.setUriProperty(IConstants.DCTERMS_CONTRIBUTOR, userUri);
	}
	

}
