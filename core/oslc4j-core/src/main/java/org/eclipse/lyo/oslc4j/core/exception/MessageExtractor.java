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
 *	   Russell Boykin		- initial API and implementation
 *	   Alberto Giammaria	- initial API and implementation
 *	   Chris Peters			- initial API and implementation
 *	   Gianluca Bernardini	- initial API and implementation
 *	   Andrew Berezovskyi   - update logging to use SLF4J
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.core.exception;

import java.text.MessageFormat;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MessageExtractor {

	private final static Logger log = LoggerFactory.getLogger(MessageExtractor.class);
	private static final String CLASS = MessageExtractor.class.getName();
	private static final String BUNDLE_NAME = "messages/oslc4jcore";
	private static ResourceBundle bundle = null;

	private MessageExtractor(){
	}

	private static synchronized ResourceBundle getMessageBundle(final Locale locale){
		if(bundle == null) {
			bundle = ResourceBundle.getBundle(BUNDLE_NAME, locale);
		}
		return bundle;
	}

	private static String getString(final Locale locale, final String key, final Object[] args ) {
		final ResourceBundle messages = getMessageBundle(locale);

		if (messages != null) {
			try {
				final String message = messages.getString( key );
				return formatMessage(locale, message, args);
			} catch ( final MissingResourceException missingResourceException ) {
				log.error("Resource {} is missing", key, missingResourceException);
				return "???" + key + "???";
			}
		}

		log.error("Resource {} is missing (as well as message bundle for the locale {})", key,
				locale);
		return "???" + key + "???";
	}

	private static String formatMessage(final Locale locale, final String message, final Object[] args) {
		final String fixedMessage = FixMessageFormat.fixPattern(message);
		return new MessageFormat(fixedMessage, locale).format(args);
	}

	public static String getMessage(final Locale locale, final String key, final Object[] params){
		return getString(locale, key, params);
	}

	public static String getMessage(final Locale locale, final String key){
		return getMessage(locale, key, null);
	}

	public static String getMessage(final String key){
		return getMessage(key, null);
	}

	public static String getMessage(final String key, final Object[] params){
		return getMessage(Locale.getDefault(), key, params);
	}
}
