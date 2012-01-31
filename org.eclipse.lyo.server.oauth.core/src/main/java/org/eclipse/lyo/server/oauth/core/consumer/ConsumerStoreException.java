package org.eclipse.lyo.server.oauth.core.consumer;

import net.oauth.OAuthException;

/**
 * Indicates an error occurred loading or saving consumer data.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 */
public class ConsumerStoreException extends OAuthException {
	private static final long serialVersionUID = 1613130197672391627L;

	public ConsumerStoreException() {
		super();
	}

	public ConsumerStoreException(String message, Throwable cause) {
		super(message, cause);
	}

	public ConsumerStoreException(String message) {
		super(message);
	}

	public ConsumerStoreException(Throwable cause) {
		super(cause);
	}
	
	public String getMessage() {
		String message = super.getMessage();
		if (message == null && getCause() != null) {
			message = getCause().getMessage();
		}
		
		if (message == null) {
			return "An error occurred.";
		}
		
		return message;
	}
}
