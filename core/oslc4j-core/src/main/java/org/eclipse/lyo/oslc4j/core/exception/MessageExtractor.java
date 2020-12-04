/*
 * Copyright (c) 2020 Contributors to the Eclipse Foundation
 *
 * See the NOTICE file(s) distributed with this work for additional
 * information regarding copyright ownership.
 *
 * This program and the accompanying materials are made available under the
 * terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0, or the Eclipse Distribution License 1.0
 * which is available at http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Clause
 */
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
