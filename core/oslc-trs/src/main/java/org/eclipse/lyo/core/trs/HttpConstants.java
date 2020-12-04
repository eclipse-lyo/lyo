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
package org.eclipse.lyo.core.trs;


/**
 * This helper utility defines commonly used string literals from the HTTP
 * specifications. See <a
 * href="http://www.w3.org/Protocols/rfc2616/rfc2616.html">RFC2616</a>. See <a
 * href="http://tools.ietf.org/html/rfc4229">RFC4229</a>.
 * @since 1.0
 */
public class HttpConstants {

	/**
	 * General-header field name for <i>Cache-Control</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9"
	 * >RFC2616 Section 14.9</a>
	 */
	public static final String CACHE_CONTROL = "Cache-Control"; //$NON-NLS-1$

	/**
	 * General-header field name for <i>Connection</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.10"
	 * >RFC2616 Section 14.10</a>
	 */
	public static final String CONNECTION = "Connection"; //$NON-NLS-1$

	/**
	 * General-header field name for <i>Date</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.18"
	 * >RFC2616 Section 14.18</a>
	 */
	public static final String DATE = "Date"; //$NON-NLS-1$

	/**
	 * General-header field name for <i>Pragma</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.32"
	 * >RFC2616 Section 14.32</a>
	 */
	public static final String PRAGMA = "Pragma"; //$NON-NLS-1$

	/**
	 * General-header field name for <i>Trailer</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.40"
	 * >RFC2616 Section 14.40</a>
	 */
	public static final String TRAILER = "Trailer"; //$NON-NLS-1$

	/**
	 * General-header field name for <i>Transfer-Encoding</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.41"
	 * >RFC2616 Section 14.41</a>
	 */
	public static final String TRANSFER_ENCODING = "Transfer-Encoding"; //$NON-NLS-1$

	/**
	 * General-header field name for <i>Upgrade</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.42"
	 * >RFC2616 Section 14.42</a>
	 */
	public static final String UPGRADE = "Upgrade"; //$NON-NLS-1$

	/**
	 * General-header field name for <i>Via</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.45"
	 * >RFC2616 Section 14.45</a>
	 */
	public static final String VIA = "Via"; //$NON-NLS-1$

	/**
	 * General-header field name for <i>Warning</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.46"
	 * >RFC2616 Section 14.46</a>
	 */
	public static final String WARNING = "Warning"; //$NON-NLS-1$

	/**
	 * Value of <i>public</i> cache response directive for {@link #CACHE_CONTROL} general-header
	 * field. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9.1"
	 * >RFC2616 Section 14.9.1</a>
	 */
	public static final String PUBLIC = "public"; //$NON-NLS-1$

	/**
	 * Value of <i>private</i> cache response directive for {@link #CACHE_CONTROL} general-header
	 * field. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9.1"
	 * >RFC2616 Section 14.9.1</a>
	 */
	public static final String PRIVATE = "private"; //$NON-NLS-1$

	/**
	 * Value of <i>no-cache</i> cache request/response directive for {@link #CACHE_CONTROL}
	 * general-header field. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9.1"
	 * >RFC2616 Section 14.9.1</a>
	 */
	public static final String NO_CACHE = "no-cache"; //$NON-NLS-1$

	/**
	 * Value of <i>no-store</i> cache request/response directive for {@link #CACHE_CONTROL}
	 * general-header field. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9.2"
	 * >RFC2616 Section 14.9.2</a>
	 */
	public static final String NO_STORE = "no-store"; //$NON-NLS-1$

	/**
	 * Value of <i>max-age</i> cache request/response directive for {@link #CACHE_CONTROL}
	 * general-header field. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9.3"
	 * >RFC2616 Section 14.9.3</a>
	 */
	public static final String MAX_AGE = "max-age"; //$NON-NLS-1$

	/**
	 * Value of <i>max-stale</i> cache request directive for {@link #CACHE_CONTROL} general-header
	 * field. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9.3"
	 * >RFC2616 Section 14.9.3</a>
	 */
	public static final String MAX_STALE = "max-stale"; //$NON-NLS-1$

	/**
	 * Value of <i>min-fresh</i> cache request directive for {@link #CACHE_CONTROL} general-header
	 * field. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9.3"
	 * >RFC2616 Section 14.9.3</a>
	 */
	public static final String MIN_FRESH = "min-fresh"; //$NON-NLS-1$

	/**
	 * Value of <i>s-maxage</i> cache response directive for {@link #CACHE_CONTROL} general-header
	 * field. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9.3"
	 * >RFC2616 Section 14.9.3</a>
	 */
	public static final String S_MAXAGE = "s-maxage"; //$NON-NLS-1$

	/**
	 * Value of <i>proxy-maxage</i> cache request directive for {@link #CACHE_CONTROL}
	 * general-header field. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9.4"
	 * >RFC2616 Section 14.9.4</a>
	 */
	public static final String PROXY_MAXAGE = HttpConstants.S_MAXAGE; // descriptive
	// alias

	/**
	 * Value of <i>only-if-cached</i> cache request directive for {@link #CACHE_CONTROL}
	 * general-header field. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9.4"
	 * >RFC2616 Section 14.9.4</a>
	 */
	public static final String ONLY_IF_CACHED = "only-if-cached"; //$NON-NLS-1$

	/**
	 * Value of <i>must-revalidate</i> cache response directive for {@link #CACHE_CONTROL}
	 * general-header field. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9.4"
	 * >RFC2616 Section 14.9.4</a>
	 */
	public static final String MUST_REVALIDATE = "must-revalidate"; //$NON-NLS-1$

	/**
	 * Value of <i>proxy-revalidate</i> cache response directive for {@link #CACHE_CONTROL}
	 * general-header field. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9.4"
	 * >RFC2616 Section 14.9.4</a>
	 */
	public static final String PROXY_REVALIDATE = "proxy-revalidate"; //$NON-NLS-1$

	/**
	 * Value of <i>no-transform</i> cache request/response directive for {@link #CACHE_CONTROL}
	 * general-header field. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9.5"
	 * >RFC2616 Section 14.9.5</a>
	 */
	public static final String NO_TRANSFORM = "no-transform"; //$NON-NLS-1$

	/**
	 * This is a custom cache control request directive for cases where the
	 * origin server doesn't include an Expires header or cache validators.
	 * Response lifetime value precedence: max_age_value expires_value -
	 * date_value x-jfs-default-lifetime
	 * Cache-Control: x-jfs-default-lifetime=&lt;seconds&gt;
	 * See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec13.html#sec13.2.4"
	 * >RFC2616 Section 13.2.4</a>
	 */
	public static final String DEFAULT_LIFETIME = "x-jfs-default-lifetime"; //$NON-NLS-1$

	/**
	 * Request-header field name for <i>Accept</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.3"
	 * >RFC2616 Section 5.3</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.1"
	 * >RFC2616 Section 14.1</a>
	 */
	public static final String ACCEPT = "Accept"; //$NON-NLS-1$

	/**
	 * Request-header field name for <i>Accept-Charset</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.3"
	 * >RFC2616 Section 5.3</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.2"
	 * >RFC2616 Section 14.2</a>
	 */
	public static final String ACCEPT_CHARSET = "Accept-Charset"; //$NON-NLS-1$

	/**
	 * Request-header field name for <i>Accept-Encoding</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.3"
	 * >RFC2616 Section 5.3</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.3"
	 * >RFC2616 Section 14.3</a>
	 */
	public static final String ACCEPT_ENCODING = "Accept-Encoding"; //$NON-NLS-1$

	/**
	 * Request-header field name for <i>Accept-Language</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.3"
	 * >RFC2616 Section 5.3</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.4"
	 * >RFC2616 Section 14.4</a>
	 */
	public static final String ACCEPT_LANGUAGE = "Accept-Language"; //$NON-NLS-1$

	/**
	 * Request-header field name for <i>Authorization</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.3"
	 * >RFC2616 Section 5.3</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.8"
	 * >RFC2616 Section 14.8</a>
	 */
	public static final String AUTHORIZATION = "Authorization"; //$NON-NLS-1$

	/**
	 * Request-header field name for <i>Expect</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.3"
	 * >RFC2616 Section 5.3</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.20"
	 * >RFC2616 Section 14.20</a>
	 */
	public static final String EXPECT = "Expect"; //$NON-NLS-1$

	/**
	 * Request-header field name for <i>From</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.3"
	 * >RFC2616 Section 5.3</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.22"
	 * >RFC2616 Section 14.22</a>
	 */
	public static final String FROM = "From"; //$NON-NLS-1$

	/**
	 * Request-header field name for <i>Host</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.3"
	 * >RFC2616 Section 5.3</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.23"
	 * >RFC2616 Section 14.23</a>
	 */
	public static final String HOST = "Host"; //$NON-NLS-1$

	/**
	 * Request-header field name for <i>If-Match</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.3"
	 * >RFC2616 Section 5.3</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.24"
	 * >RFC2616 Section 14.24</a>
	 */
	public static final String IF_MATCH = "If-Match"; //$NON-NLS-1$

	/**
	 * Request-header field name for <i>If-Modified-Since</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.3"
	 * >RFC2616 Section 5.3</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.25"
	 * >RFC2616 Section 14.25</a>
	 */
	public static final String IF_MODIFIED_SINCE = "If-Modified-Since"; //$NON-NLS-1$

	/**
	 * Request-header field name for <i>If-None-Match</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.3"
	 * >RFC2616 Section 5.3</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.26"
	 * >RFC2616 Section 14.26</a>
	 */
	public static final String IF_NONE_MATCH = "If-None-Match"; //$NON-NLS-1$

	/**
	 * Request-header field name for <i>If-Range</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.3"
	 * >RFC2616 Section 5.3</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.27"
	 * >RFC2616 Section 14.27</a>
	 */
	public static final String IF_RANGE = "If-Range"; //$NON-NLS-1$

	/**
	 * Request-header field name for <i>If-Unmodified-Since</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.3"
	 * >RFC2616 Section 5.3</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.28"
	 * >RFC2616 Section 14.28</a>
	 */
	public static final String IF_UNMODIFIED_SINCE = "If-Unmodified-Since"; //$NON-NLS-1$

	/**
	 * Response-header field name for <i>Link</i>. See <a
	 * href="http://tools.ietf.org/html/rfc5988#section-5">RFC5988 Section 5</a>
	 */
	public static final String LINK = "Link"; //$NON-NLS-1$

	/**
	 * Request-header field name for <i>Max-Forwards</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.3"
	 * >RFC2616 Section 5.3</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.31"
	 * >RFC2616 Section 14.31</a>
	 */
	public static final String MAX_FORWARDS = "Max-Forwards"; //$NON-NLS-1$

	/**
	 * Request-header field name for <i>Proxy-Authorization</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.3"
	 * >RFC2616 Section 5.3</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.34"
	 * >RFC2616 Section 14.34</a>
	 */
	public static final String PROXY_AUTHORIZATION = "Proxy-Authorization"; //$NON-NLS-1$

	/**
	 * Non-standard request-header field name for <i>X-Method-Override</i>.
	 */
	public static final String X_METHOD_OVERRIDE = "X-Method-Override"; //$NON-NLS-1$

	/**
	 * Non-standard request-header field name for <i>X-Last-Modified-XSD</i>.
	 */
	public static final String X_LAST_MODIFIED_XSD = "X-Last-Modified-XSD"; //$NON-NLS-1$

	/**
	 * Non-standard request-header field name for <i>X-Last-Modified-User</i>.
	 */
	public static final String X_LAST_MODIFIED_USER = "X-Last-Modified-User"; //$NON-NLS-1$

	/**
	 * Response-header field name for <i>Set-Cookie</i>. See <a
	 * href="http://tools.ietf.org/html/rfc4229#section-2.1.96">RFC4229 Section
	 * 2.1.96</a>.
	 */
	public static final String SET_COOKIE = "Set-Cookie"; //$NON-NLS-1$

	/**
	 * Request-header field name for <i>Accept</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.3"
	 * >RFC2616 Section 5.3</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.39"
	 * >RFC2616 Section 14.39</a>
	 */
	public static final String TE = "TE"; //$NON-NLS-1$

	/**
	 * Non-standard request-header field name for <i>Name</i>.
	 */
	public static final String NAME_HEADER = "Name"; //$NON-NLS-1$

	/**
	 * Non-standard request-header field name for <i>X-Jazz-Owning-Context</i>.
	 */
	public static final String SECURITY_CONTEXT_HEADER = "X-Jazz-Owning-Context"; //$NON-NLS-1$

	/**
	 * Response-header field name for <i>Accept-Ranges</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec6.html#sec6.2"
	 * >RFC2616 Section 6.2</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.5"
	 * >RFC2616 Section 14.5</a>
	 */
	public static final String ACCEPT_RANGES = "Accept-Ranges"; //$NON-NLS-1$

	/**
	 * Response-header field name for <i>Age</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec6.html#sec6.2"
	 * >RFC2616 Section 6.2</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.6"
	 * >RFC2616 Section 14.6</a>
	 */
	public static final String AGE = "Age"; //$NON-NLS-1$

	/**
	 * Response-header field name for <i>ETag</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec6.html#sec6.2"
	 * >RFC2616 Section 6.2</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.19"
	 * >RFC2616 Section 14.19</a>
	 */
	public static final String ETAG = "ETag"; //$NON-NLS-1$

	/**
	 * Response-header field name for <i>Location</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec6.html#sec6.2"
	 * >RFC2616 Section 6.2</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.30"
	 * >RFC2616 Section 14.30</a>
	 */
	public static final String LOCATION = "Location"; //$NON-NLS-1$

	/**
	 * Response-header field name for <i>Proxy-Authenticate</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec6.html#sec6.2"
	 * >RFC2616 Section 6.2</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.33"
	 * >RFC2616 Section 14.33</a>
	 */
	public static final String PROXY_AUTHENTICATE = "Proxy-Authenticate"; //$NON-NLS-1$

	/**
	 * Response-header field name for <i>Retry-After</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec6.html#sec6.2"
	 * >RFC2616 Section 6.2</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.37"
	 * >RFC2616 Section 14.37</a>
	 */
	public static final String RETRY_AFTER = "Retry-After"; //$NON-NLS-1$

	/**
	 * Response-header field name for <i>Server</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec6.html#sec6.2"
	 * >RFC2616 Section 6.2</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.38"
	 * >RFC2616 Section 14.38</a>
	 */
	public static final String SERVER = "Server"; //$NON-NLS-1$

	/**
	 * Response-header field name for <i>Vary</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec6.html#sec6.2"
	 * >RFC2616 Section 6.2</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.44"
	 * >RFC2616 Section 14.44</a>
	 */
	public static final String VARY = "Vary"; //$NON-NLS-1$

	/**
	 * Response-header field name for <i>Vary</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec6.html#sec6.2"
	 * >RFC2616 Section 6.2</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.47"
	 * >RFC2616 Section 14.47</a>
	 */
	public static final String WWW_AUTHENTICATE = "WWW-Authenticate"; //$NON-NLS-1$

	/**
	 * Response-header field name for <i>Content-Disposition</i>. See <a
	 * href="http://tools.ietf.org/html/rfc4229#section-2.1.22">RFC4229 Section
	 * 2.1.22</a>
	 */
	public static final String CONTENT_DISPOSITION = "Content-Disposition"; //$NON-NLS-1$

	/**
	 * Non-standard response-header field name for <i>X-Last-Modified-User</i>.
	 */
	public static final String LAST_MODIFIED_USER = "X-Last-Modified-User"; //$NON-NLS-1$

	/**
	 * Non-standard response-header field name for <i>X-Last-Modified-XSD</i>.
	 */
	public static final String LAST_MODIFIED_XSD = "X-Last-Modified-XSD"; //$NON-NLS-1$

	/**
	 * Non-standard response-header field name for <i>X-jazz-web-oauth-url</i>.
	 */
	public static final String X_OAUTH_AUTHORIZATION_URL = "X-jazz-web-oauth-url"; //$NON-NLS-1$

	/**
	 * Non-standard response-header field name for <i>X-Jazz-Authorization-Guidance</i>.
	 */
	public static final String X_JAZZ_AUTHORIZATION_GUIDANCE = "X-Jazz-Authorization-Guidance"; //$NON-NLS-1$

	/**
	 * Non-standard response-header field name for <i>X-Jazz-Creator</i>, used
	 * to specify the value for the <i>dc:creator</i> property for the import
	 * service.
	 */
	public static final String X_JAZZ_CREATOR = "X-Jazz-Creator"; //$NON-NLS-1$

	/**
	 * Non-standard response-header field name for <i>X-Jazz-Created</i>, used
	 * to specify the value for the <i>dc:created</i> property for the import
	 * service.
	 */
	public static final String X_JAZZ_CREATED = "X-Jazz-Created"; //$NON-NLS-1$

	/**
	 * Non-standard response-header field name for <i>X-Jazz-Contributor</i>,
	 * used to specify the value for the <i>dc:contributor</i> property for the
	 * import service.
	 */
	public static final String X_JAZZ_CONTRIBUTOR = "X-Jazz-Contributor"; //$NON-NLS-1$

	/**
	 * Non-standard response-header field name for <i>X-Jazz-Modified</i>, used
	 * to specify the value for the <i>dc:modified</i> property for the import
	 * service.
	 */
	public static final String X_JAZZ_MODIFIED = "X-Jazz-Modified"; //$NON-NLS-1$

	/**
	 * Non-standard response-header field name for <i>X-Jazz-Current</i>, used
	 * to specify that a revision being imported is current, and so should be
	 * indexed.
	 */
	public static final String X_JAZZ_CURRENT = "X-Jazz-Current"; //$NON-NLS-1$

	/**
	 * Non-standard response-header field name for <i>X-Jazz-Revision</i>, used
	 * to specify the revision identifier for an imported revision.
	 */
	public static final String X_JAZZ_REVISION = "X-Jazz-Revision"; //$NON-NLS-1$

	/**
	 * Non-standard response-header field name for <i>X-Jazz-Archived</i>, used
	 * to specify that the revision being imported should be marked archived
	 * (deleted).
	 */
	public static final String X_JAZZ_ARCHIVED = "X-Jazz-Archived"; //$NON-NLS-1$

	/**
	 * Non-standard response-header field name for <i>X-Jazz-Force-Ordering</i>,
	 * used to specify that the individual parts of a bulk operations response
	 * are in the same order as the request parts.
	 * @since 3.0.1
	 */
	public static final String X_JAZZ_FORCE_ORDERING = "X-Jazz-Force-Ordering"; //$NON-NLS-1$

	/**
	 * Non-standard request-header field name for <i>X-Jazz-CSRF-Prevent</i>,
	 * used to prevent cross-site request forgeries
	 */
	public static final String X_JAZZ_CSRF_PREVENT = "X-Jazz-CSRF-Prevent"; //$NON-NLS-1$

	/**
	 * Non-standard response-header field name for <i>OSLC-Core-Version</i>,
	 * used to specify that the version of the OSLC Core Specification.
	 */
	public static final String OSLC_CORE_VER = "OSLC-Core-Version"; //$NON-NLS-1$

	/**
	 * Entity-header field name for <i>Allow</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html#sec7.1"
	 * >RFC2616 Section 7.1</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.7"
	 * >RFC2616 Section 14.7</a>
	 */
	public static final String ALLOW = "Allow"; //$NON-NLS-1$

	/**
	 * Entity-header field name for <i>Cookie</i>. See <a
	 * href="http://tools.ietf.org/html/rfc4229#section-2.1.34">RFC4229 Section
	 * 2.1.34</a>
	 */
	public static final String COOKIE = "Cookie"; //$NON-NLS-1$

	/**
	 * Entity-header field name for <i>Content-Encoding</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html#sec7.1"
	 * >RFC2616 Section 7.1</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.11"
	 * >RFC2616 Section 14.11</a>
	 */
	public static final String CONTENT_ENCODING = "Content-Encoding"; //$NON-NLS-1$

	/**
	 * Entity-header field name for <i>Content-Language</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html#sec7.1"
	 * >RFC2616 Section 7.1</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.12"
	 * >RFC2616 Section 14.12</a>
	 */
	public static final String CONTENT_LANGUAGE = "Content-Language"; //$NON-NLS-1$

	/**
	 * Entity-header field name for <i>Content-Length</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html#sec7.1"
	 * >RFC2616 Section 7.1</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13"
	 * >RFC2616 Section 14.13</a>
	 */
	public static final String CONTENT_LENGTH = "Content-Length"; //$NON-NLS-1$

	/**
	 * Entity-header field name for <i>Content-Location</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html#sec7.1"
	 * >RFC2616 Section 7.1</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.14"
	 * >RFC2616 Section 14.14</a>
	 */
	public static final String CONTENT_LOCATION = "Content-Location"; //$NON-NLS-1$

	/**
	 * Entity-header field name for <i>Content-MD5</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html#sec7.1"
	 * >RFC2616 Section 7.1</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.15"
	 * >RFC2616 Section 14.15</a>
	 */
	public static final String CONTENT_MD5 = "Content-MD5"; //$NON-NLS-1$

	/**
	 * Entity-header field name for <i>Content-Range</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html#sec7.1"
	 * >RFC2616 Section 7.1</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.16"
	 * >RFC2616 Section 14.16</a>
	 */
	public static final String CONTENT_RANGE = "Content-Range"; //$NON-NLS-1$

	/**
	 * Entity-header field name for <i>Content-Type</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html#sec7.1"
	 * >RFC2616 Section 7.1</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17"
	 * >RFC2616 Section 14.17</a>
	 */
	public static final String CONTENT_TYPE = "Content-Type"; //$NON-NLS-1$

	/**
	 * Entity-header field name for <i>Expires</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html#sec7.1"
	 * >RFC2616 Section 7.1</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.21"
	 * >RFC2616 Section 14.21</a>
	 */
	public static final String EXPIRES = "Expires"; //$NON-NLS-1$

	/**
	 * Entity-header field name for <i>Last-Modified</i>. See <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html#sec7.1"
	 * >RFC2616 Section 7.1</a> and <a
	 * href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.29"
	 * >RFC2616 Section 14.29</a>
	 */
	public static final String LAST_MODIFIED = "Last-Modified"; //$NON-NLS-1$

	/**
	 * Entity-header field name for <i>Cookie</i>. See <a
	 * href="http://tools.ietf.org/html/rfc4229#section-2.1.50">RFC4229 Section
	 * 2.1.50</a>
	 */
	public static final String KEEP_ALIVE = "Keep-Alive"; //$NON-NLS-1$

	/**
	 * Literal string value for HTTP wildcard character.
	 */
	public static final String WILDCARD = "*"; //$NON-NLS-1$

	/**
	 * Literal string value for the prefix used on entity tags to denote weak
	 * validation.
	 */
	public static final String WEAK_ETAG_PREFIX = "W/"; //$NON-NLS-1$

	/**
	 * Literal string value of the encoding format produced by the <i>gzip</i>
	 * file compression program.
	 */
	public static final String GZIP = "gzip"; //$NON-NLS-1$

	/**
	 * Literal string value of the abbreviation for the Secure Sockets Layer
	 * protocol.
	 */
	public static final String SSL = "SSL"; //$NON-NLS-1$

	/**
	 * Literal string value of the abbreviation for the Hypertext Transfer
	 * Protocol.
	 */
	public static final String HTTP = "http"; //$NON-NLS-1$

	/**
	 * Literal string value of the abbreviation for the Hypertext Transfer
	 * Protocol Secure.
	 */
	public static final String HTTPS = "https"; //$NON-NLS-1$

	/**
	 * Literal string value of a forward slash.
	 */
	public static final String FORWARD_SLASH = "/"; //$NON-NLS-1$

	/**
	 * Literal string value of a backwards slash.
	 */
	public static final String BACKWARDS_SLASH = "\\"; //$NON-NLS-1$

	/**
	 * Value for the <i>close</i> option of the {@link #CONNECTION} header
	 * field.
	 */
	public static final String CLOSE = "Close"; //$NON-NLS-1$

	/**
	 * Name of the <i>charset</i> parameter in the {@link #CONTENT_TYPE} header
	 * field.
	 */
	public static final String CHARSET = "charset"; //$NON-NLS-1$

	/**
	 * Literal string value of commonly used parameter value.
	 */
	public static final String OK = "OK"; //$NON-NLS-1$

	/**
	 * Literal string value of an ampersand.
	 */
	public static final String AMPERSAND = "&"; //$NON-NLS-1$

	/**
	 * Literal string value of a colon.
	 */
	public static final String COLON = ":"; //$NON-NLS-1$

	/**
	 * Literal string value for a ://S
	 */
	public static final String COLON_SLASH_SLASH = "://"; //$NON-NLS-1$

	/**
	 * Literal string value of a comma.
	 */
	public static final String COMMA = ","; //$NON-NLS-1$

	/**
	 * Literal string value of an empty string.
	 */
	public static final String EMPTY = ""; //$NON-NLS-1$

	/**
	 * Literal string value of an equals character.
	 */
	public static final String EQUALS = "="; //$NON-NLS-1$

	/**
	 * Literal string value of a hash character.
	 */
	public static final String HASH = "#"; //$NON-NLS-1$

	/**
	 * Literal string value of a period.
	 */
	public static final String PERIOD = "."; //$NON-NLS-1$

	/**
	 * Literal string value of a plus character.
	 */
	public static final String PLUS = "+"; //$NON-NLS-1$

	/**
	 * Literal string value of a question mark.
	 */
	public static final String QUESTION = "?"; //$NON-NLS-1$

	/**
	 * Literal string value of a forward slash.
	 */
	public static final String SLASH = "/"; //$NON-NLS-1$

	/**
	 * Literal string value of a whitespace character.
	 */
	public static final String SPACE = " "; //$NON-NLS-1$

	/**
	 * Literal string value of an asterick character.
	 */
	public static final String STAR = "*"; //$NON-NLS-1$

	/**
	 * Literal string value of a semi-colon.
	 */
	public static final String SEMI_COLON = ";"; //$NON-NLS-1$

	public static final String CONTEXT = "context"; //$NON-NLS-1$
	public static final String DEFAULT_ENCODING = "UTF-8"; //$NON-NLS-1$
	public static final int DEFAULT_PAGE_SIZE = 100;
	public static final String PAGE_SIZE = "pageSize"; //$NON-NLS-1$
	public static final String QUERY = "query"; //$NON-NLS-1$
	public static final String SEARCH = "q"; //$NON-NLS-1$
	public static final String SIZE = "size"; //$NON-NLS-1$
	/** @since 3.0.1 */
	public static final String ITERATE = "iterate"; //$NON-NLS-1$
	/** @since 3.0.1 */
	public static final String URL = "url"; //$NON-NLS-1$
	/** @since 3.0.1 */
	public static final String REVISION = "revision"; //$NON-NLS-1$

	/**
	 * The literal string value of the whitespace character convert to a URL
	 * encoded value.
	 */
	public static final String URL_ENCODED_SPACE = "%20"; //$NON-NLS-1$

	/**
	 * The literal string value of the plus character convert to a URL encoded
	 * value.
	 */
	public static final String URL_ENCODED_PLUS = "%2B"; //$NON-NLS-1$

	/**
	 * The internet media type or MIME type or Content-Type value for "*"
	 * XML feed.
	 */
	public static final String CT_ANY = "*"; //$NON-NLS-1$

	/**
	 * The internet media type or MIME type or Content-Type value for "*\/*"
	 * XML feed.
	 */
	public static final String CT_ANY_ANY = "*/*"; //$NON-NLS-1$

	/**
	 * The internet media type or MIME type or Content-Type value for an ATOM
	 * XML feed.
	 */
	public static final String CT_APPLICATION_ATOM_XML = "application/atom+xml"; //$NON-NLS-1$

	/**
	 * The internet media type or MIME type or Content-Type value for
	 * javascript.
	 */
	public static final String CT_APPLICATION_JAVASCRIPT = "application/javascript"; //$NON-NLS-1$

	/**
	 * The internet media type or MIME type or Content-Type value for JSON data.
	 */
	public static final String CT_APPLICATION_JSON = "application/json"; //$NON-NLS-1$

	/**
	 * The internet media type or MIME type or Content-Type value for an JSON
	 * request.
	 */
	public static final String CT_APPLICATION_JSONREQUEST = "application/jsonrequest"; //$NON-NLS-1$

	/**
	 * The internet media type or MIME type or Content-Type value for the human
	 * readable RDF triples format.
	 */
	public static final String CT_APPLICATION_NTRIPLES = "application/ntriples"; //$NON-NLS-1$

	/**
	 * The internet media type or MIME type or Content-Type value for a SPARQL
	 * query.
	 */
	public static final String CT_APPLICATION_SPARQL_QUERY = "application/sparql-query"; //$NON-NLS-1$

	/**
	 * The internet media type or MIME type or Content-Type value for a SPARQL result set in json
	 * format.
	 */
	public static final String CT_APPLICATION_SPARQL_RESULTS_JSON =
			"application/sparql-results+json"; //$NON-NLS-1$

	/**
	 * The internet media type or MIME type or Content-Type value for a SPARQL result set in xml
	 * format.
	 */
	public static final String CT_APPLICATION_SPARQL_RESULTS_XML = "application/sparql-results+xml"; //$NON-NLS-1$

	/**
	 * The internet media type or MIME type or Content-Type value for the Terse
	 * RDF Triple Language.
	 */
	public static final String CT_APPLICATION_TURTLE = "application/x-turtle"; //$NON-NLS-1$

	/**
	 * The internet media type or MIME type or Content-Type value for an XML
	 * feed.
	 */
	public static final String CT_APPLICATION_RDF_XML = "application/rdf+xml"; //$NON-NLS-1$

	/**
	 * The internet media type or MIME type or Content-Type value for an XML
	 * document.
	 */
	public static final String CT_APPLICATION_XML = "application/xml"; //$NON-NLS-1$

	/**
	 * The internet media type or MIME type or Content-Type value for an zip archive.
	 */
	public static final String CT_APPLICATION_ZIP = "application/zip"; //$NON-NLS-1$

	/**
	 * The internet media type or MIME type or Content-Type value for form post
	 * submissions URL encoded as name/value pairs.
	 */
	public static final String CT_APPLICATION_URL_ENCODED = "application/x-www-form-urlencoded"; //$NON-NLS-1$

	/**
	 * The internet media type or MIME type or Content-Type value for the
	 * eXtensible Hypertext Markup Language.
	 */
	public static final String CT_APPLICATION_XHTML = "application/xhtml+xml"; //$NON-NLS-1$

	public static final String CT_APPLICATION_INDEX_SPEC =
			"application/net.jazz.foundation.indexer+xml"; //$NON-NLS-1$

	/**
	 * The internet media type or MIME type or Content-Type value for an JSON
	 * data as text.
	 */
	public static final String CT_TEXT_JSON = "text/json"; //$NON-NLS-1$

	/**
	 * The internet media type or MIME type or Content-Type value for an RDF
	 * triples as text.
	 */
	public static final String CT_TEXT_N3 = "text/n3"; //$NON-NLS-1$

	/**
	 * The internet media type or MIME type or Content-Type value for text.
	 */
	public static final String CT_TEXT_PLAIN = "text/plain"; //$NON-NLS-1$

	/**
	 * The internet media type or MIME type or Content-Type value for Terse RDF
	 * Triple Language as text.
	 */
	public static final String CT_TEXT_TURTLE = "text/turtle"; //$NON-NLS-1$

	/**
	 * The internet media type or MIME type or Content-Type value for an HTML as
	 * text.
	 */
	public static final String CT_TEXT_HTML = "text/html"; //$NON-NLS-1$

	/**
	 * The internet media type or MIME type or Content-Type value for an XML as
	 * text.
	 */
	public static final String CT_TEXT_XML = "text/xml"; //$NON-NLS-1$

	/**
	 * The internet media type or MIME type or Content-Type value for CSS as
	 * text.
	 */
	public static final String CT_TEXT_CSS = "text/css"; //$NON-NLS-1$

	/**
	 * The internet media type or MIME type or Content-Type value for JavaScript
	 * as text.
	 */
	public static final String CT_TEXT_JAVASCRIPT = "text/javascript"; //$NON-NLS-1$

	/**
	 * The internet media type or MIME type or Content-Type value for an XML as
	 * RDF.
	 */
	public static final String CT_RDF_XML = "RDF/XML"; //$NON-NLS-1$

	/**
	 * HTTP method names, as documented by RFC2616, Section 9
	 */
	public enum HttpMethod {
		OPTIONS("OPTIONS"), //$NON-NLS-1$
		GET("GET"), //$NON-NLS-1$
		HEAD("HEAD"), //$NON-NLS-1$
		POST("POST"), //$NON-NLS-1$
		PUT("PUT"), //$NON-NLS-1$
		DELETE("DELETE"), //$NON-NLS-1$
		TRACE("TRACE"), //$NON-NLS-1$
		CONNECT("CONNECT"); //$NON-NLS-1$

		private HttpMethod(final String name) {
			_name = name;
		}

		@Override
		public String toString() {
			return _name;
		}

		/**
		 * Constructs a HttpMethod from the referenced name, assumed to be the
		 * result of calling {@link #toString()} on an HttpMethod instance. This
		 * differs from {@link #valueOf(String)} in that method expects the
		 * declarative name (i.e the result of {@link #name()}).
		 * @param name
		 *			  The name of the HttpMethod to search for. May be <code>null</code>. If
		 *			  <code>null</code>, <code>null</code> is returned.
		 * @return The MediaType whose {@link #toString()} value equals that of <code>name</code>,
		 *		   else <code>null</code>.
		 */
		static public HttpMethod fromString(String name) {
			if (name == null)
				return null;
			for (HttpMethod mType : HttpMethod.values()) {
				if (mType.toString().equalsIgnoreCase(name))
					return mType;
			}
			return null;
		}

		private final String _name;
	}

}