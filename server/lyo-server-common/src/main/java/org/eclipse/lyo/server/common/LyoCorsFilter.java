/*
 * Copyright (c) 2026 Contributors to the Eclipse Foundation
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

package org.eclipse.lyo.server.common;

import jakarta.annotation.Priority;
import jakarta.inject.Inject;
import jakarta.ws.rs.Priorities;
import jakarta.ws.rs.container.ContainerRequestContext;
import jakarta.ws.rs.container.ContainerRequestFilter;
import jakarta.ws.rs.container.ContainerResponseContext;
import jakarta.ws.rs.container.ContainerResponseFilter;
import jakarta.ws.rs.container.PreMatching;
import jakarta.ws.rs.core.MultivaluedMap;
import jakarta.ws.rs.core.Response;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * CORS support for Lyo server apps.
 *
 * <p>Allowed origins come from {@link LyoAppConfiguration#corsFriends()}: empty disables CORS,
 * <code>*</code> allows any origin, otherwise only the listed origins (e.g. <code>
 * https://rm.example.com</code>) are allowed.
 *
 * <p>Preflight requests are answered by this filter before resource matching and before any
 * other request filter (authentication, ...) runs, so those filters never need to
 * special-case <code>OPTIONS</code>. A preflight from an origin that is not allowed gets a 403.
 */
@PreMatching
@Priority(Priorities.AUTHENTICATION - 100)
public class LyoCorsFilter implements ContainerRequestFilter, ContainerResponseFilter {
  private static final Logger log = LoggerFactory.getLogger(LyoCorsFilter.class);

  public static final String ORIGIN = "Origin";
  public static final String VARY = "Vary";
  public static final String ACCESS_CONTROL_REQUEST_METHOD = "Access-Control-Request-Method";
  public static final String ACCESS_CONTROL_ALLOW_ORIGIN = "Access-Control-Allow-Origin";
  public static final String ACCESS_CONTROL_ALLOW_CREDENTIALS = "Access-Control-Allow-Credentials";
  public static final String ACCESS_CONTROL_ALLOW_METHODS = "Access-Control-Allow-Methods";
  public static final String ACCESS_CONTROL_ALLOW_HEADERS = "Access-Control-Allow-Headers";
  public static final String ACCESS_CONTROL_EXPOSE_HEADERS = "Access-Control-Expose-Headers";
  public static final String ACCESS_CONTROL_MAX_AGE = "Access-Control-Max-Age";

  private static final String WILDCARD = "*";
  private static final int MAX_LOGGED_VALUE_LENGTH = 200;

  private final Set<String> allowedOrigins = new TreeSet<>();
  private final boolean allowAnyOrigin;

  @Inject
  public LyoCorsFilter(LyoAppConfiguration configuration) {
    if (configuration.corsFriends() != null) {
      configuration.corsFriends().stream()
          .filter(origin -> origin != null && !origin.isBlank())
          .map(String::trim)
          .forEach(allowedOrigins::add);
    }
    allowAnyOrigin = allowedOrigins.remove(WILDCARD);

    if (allowAnyOrigin) {
      // TODO (future hardening): '*' echoes back any origin together with
      //  Access-Control-Allow-Credentials, so any website can make requests on behalf of a
      //  logged-in user. Kept for backward compatibility; consider requiring an explicit list of
      //  origins, or refusing credentials for '*'.
      log.warn(
          "CORS: ANY origin is allowed ('*'), with credentials. Any website visited by a"
              + " logged-in user can read and modify data through this server on that user's"
              + " behalf. Configure an explicit list of origins instead.");
    } else if (allowedOrigins.isEmpty()) {
      log.info("CORS: disabled - no allowed origins configured.");
    } else {
      log.info("CORS: enabled for origins {}", allowedOrigins);
    }
  }

  @Override
  public void filter(ContainerRequestContext request) {
    if (isDisabled() || !isPreflight(request)) {
      return;
    }
    // The CORS headers, and the warning for a rejected origin, are added by the response filter.
    if (isOriginAllowed(request.getHeaderString(ORIGIN))) {
      request.abortWith(Response.ok().build());
    } else {
      request.abortWith(Response.status(Response.Status.FORBIDDEN).build());
    }
  }

  @Override
  public void filter(ContainerRequestContext request, ContainerResponseContext response) {
    String origin = request.getHeaderString(ORIGIN);
    if (isDisabled() || origin == null) {
      return;
    }
    if (!isOriginAllowed(origin)) {
      log.warn(
          "CORS: origin '{}' is not allowed ({} {}). Allowed origins: {}",
          sanitize(origin),
          request.getMethod(),
          sanitize(request.getUriInfo().getRequestUri().getPath()),
          allowAnyOrigin ? WILDCARD : allowedOrigins);
      return;
    }

    MultivaluedMap<String, Object> headers = response.getHeaders();
    headers.add(VARY, ORIGIN);
    headers.putSingle(ACCESS_CONTROL_ALLOW_ORIGIN, origin);
    headers.putSingle(ACCESS_CONTROL_ALLOW_CREDENTIALS, "true");
    if (isPreflight(request)) {
      headers.putSingle(ACCESS_CONTROL_ALLOW_METHODS, String.join(", ", getAllowedMethods()));
      headers.putSingle(ACCESS_CONTROL_ALLOW_HEADERS, String.join(", ", getAllowedHeaders()));
      headers.putSingle(ACCESS_CONTROL_MAX_AGE, String.valueOf(getMaxAgeSeconds()));
    } else {
      headers.putSingle(ACCESS_CONTROL_EXPOSE_HEADERS, String.join(", ", getExposedHeaders()));
    }
  }

  /** Methods a cross-origin request may use. */
  protected List<String> getAllowedMethods() {
    return List.of("GET", "POST", "PUT", "DELETE", "OPTIONS", "HEAD");
  }

  /** Non-safelisted request headers a cross-origin request may send. */
  protected List<String> getAllowedHeaders() {
    return List.of(
        "Origin",
        "Content-Type",
        "Accept",
        "Authorization",
        "OSLC-Core-Version",
        "Configuration-Context",
        "OSLC-Configuration-Context",
        "If-Match",
        "If-None-Match", // To allow caching
        "If-Modified-Since", // To allow caching
        "Prefer"); // To allow OSLC Preview headers
  }

  /** Response headers a cross-origin page may read, beyond the safelisted ones. */
  protected List<String> getExposedHeaders() {
    return List.of(
        "ETag", // To allow caching
        "Accept-Post", // To allow caching
        "Link", // To support LDP & TRS
        "Preference-Applied", // To allow OSLC Preview headers, and to support LDP & TRS
        "Location",
        "OSLC-Core-Version");
  }

  /** How long a browser may cache a preflight result. */
  protected int getMaxAgeSeconds() {
    return 3600;
  }

  private boolean isDisabled() {
    return !allowAnyOrigin && allowedOrigins.isEmpty();
  }

  private boolean isOriginAllowed(String origin) {
    return allowAnyOrigin || allowedOrigins.contains(origin.trim());
  }

  private static boolean isPreflight(ContainerRequestContext request) {
    return "OPTIONS".equalsIgnoreCase(request.getMethod())
        && request.getHeaderString(ORIGIN) != null
        && request.getHeaderString(ACCESS_CONTROL_REQUEST_METHOD) != null;
  }

  /** Client-supplied values are logged without line breaks and truncated. */
  static String sanitize(String value) {
    String cleaned = value.replaceAll("\\p{Cntrl}", "?");
    return cleaned.length() > MAX_LOGGED_VALUE_LENGTH
        ? cleaned.substring(0, MAX_LOGGED_VALUE_LENGTH) + "..."
        : cleaned;
  }
}
