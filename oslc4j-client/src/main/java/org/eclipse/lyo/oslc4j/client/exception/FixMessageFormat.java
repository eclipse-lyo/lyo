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
 *     Russell Boykin       - initial API and implementation
 *     Alberto Giammaria    - initial API and implementation
 *     Chris Peters         - initial API and implementation
 *     Gianluca Bernardini  - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.client.exception;

public class FixMessageFormat {

	private static final char SINGLE_QUOTE = '\''; //@01A3
	private static final char CURLY_BRACE_LEFT = '{';
	private static final char CURLY_BRACE_RIGHT = '}';

	private static final int STATE_INITIAL = 0; //@01A4
	private static final int STATE_SINGLE_QUOTE = 1;
	private static final int STATE_LITERAL_BRACE = 2;
	private static final int STATE_MSG_ELEMENT = 3;

	public static String fixPattern(final String pattern) { //@01
		final StringBuffer buf = new StringBuffer(pattern.length() * 2);
		int state = STATE_INITIAL;
		for (int i = 0, j = pattern.length(); i < j; ++i) {
			final char c = pattern.charAt(i);
			switch (state) {
				case STATE_INITIAL :
					switch (c) {
						case SINGLE_QUOTE :
							state = STATE_SINGLE_QUOTE;
							break;
						case CURLY_BRACE_LEFT :
							state = STATE_MSG_ELEMENT;
							break;
					}
					break;
				case STATE_SINGLE_QUOTE :
					switch (c) {
						case SINGLE_QUOTE :
							state = STATE_INITIAL;
							break;
						case CURLY_BRACE_LEFT :
						case CURLY_BRACE_RIGHT :
							state = STATE_LITERAL_BRACE;
							break;
						default :
							buf.append(SINGLE_QUOTE);
							state = STATE_INITIAL;
					}
					break;
				case STATE_LITERAL_BRACE :
					switch (c) {
						case SINGLE_QUOTE :
							state = STATE_INITIAL;
							break;
					}
					break;
				case STATE_MSG_ELEMENT :
					switch (c) {
						case CURLY_BRACE_RIGHT :
							state = STATE_INITIAL;
							break;
					}
					break;
				default : // This will not happen.
			}
			buf.append(c);
		}
		// End of scan
		if (state == STATE_SINGLE_QUOTE) {
			buf.append(SINGLE_QUOTE);
		}
		return new String(buf);
	}

}
