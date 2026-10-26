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

import static org.eclipse.lyo.server.common.LyoCorsFilter.*;
import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

import jakarta.ws.rs.container.ContainerRequestContext;
import jakarta.ws.rs.container.ContainerResponseContext;
import jakarta.ws.rs.core.MultivaluedHashMap;
import jakarta.ws.rs.core.MultivaluedMap;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.UriInfo;
import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import org.junit.Test;
import org.mockito.ArgumentCaptor;

public class LyoCorsFilterTest {

  private static final String RM = "https://rm.example.com";

  /** Runs a request through both filter phases, as Jersey would. */
  private static class Exchange {
    final ContainerRequestContext request = mock(ContainerRequestContext.class);
    final ContainerResponseContext response = mock(ContainerResponseContext.class);
    final MultivaluedMap<String, Object> responseHeaders = new MultivaluedHashMap<>();
    Response aborted;

    Exchange(String method, Map<String, String> headers) {
      when(request.getMethod()).thenReturn(method);
      when(request.getHeaderString(any())).thenAnswer(i -> headers.get(i.<String>getArgument(0)));
      UriInfo uriInfo = mock(UriInfo.class);
      when(uriInfo.getRequestUri()).thenReturn(URI.create("https://connector.example.com/r"));
      when(request.getUriInfo()).thenReturn(uriInfo);
      when(response.getHeaders()).thenReturn(responseHeaders);
    }

    Exchange run(LyoCorsFilter filter) {
      filter.filter(request);
      ArgumentCaptor<Response> captor = ArgumentCaptor.forClass(Response.class);
      verify(request, atMost(1)).abortWith(captor.capture());
      if (!captor.getAllValues().isEmpty()) {
        aborted = captor.getValue();
      }
      filter.filter(request, response);
      return this;
    }

    Object header(String name) {
      return responseHeaders.getFirst(name);
    }
  }

  private static LyoCorsFilter filter(String... friends) {
    return new LyoCorsFilter(
        new LyoAppConfiguration("https://connector.example.com", "", Set.of(friends)));
  }

  private static Exchange get(String origin) {
    Map<String, String> headers = new HashMap<>();
    if (origin != null) {
      headers.put(ORIGIN, origin);
    }
    return new Exchange("GET", headers);
  }

  private static Exchange preflight(String origin) {
    return new Exchange(
        "OPTIONS", Map.of(ORIGIN, origin, ACCESS_CONTROL_REQUEST_METHOD, "PUT"));
  }

  @Test
  public void allowedOriginGetsCorsHeaders() {
    Exchange e = get(RM).run(filter(RM));
    assertNull(e.aborted);
    assertEquals(RM, e.header(ACCESS_CONTROL_ALLOW_ORIGIN));
    assertEquals("true", e.header(ACCESS_CONTROL_ALLOW_CREDENTIALS));
    assertEquals(ORIGIN, e.header(VARY));
    assertTrue(e.header(ACCESS_CONTROL_EXPOSE_HEADERS).toString().contains("ETag"));
    assertNull(e.header(ACCESS_CONTROL_ALLOW_METHODS));
  }

  @Test
  public void rejectedOriginGetsNoCorsHeaders() {
    Exchange e = get("https://evil.example.com").run(filter(RM));
    assertNull(e.aborted);
    assertTrue(e.responseHeaders.isEmpty());
  }

  @Test
  public void allowedPreflightIsAnsweredWithAllowHeaders() {
    Exchange e = preflight(RM).run(filter(RM));
    assertEquals(200, e.aborted.getStatus());
    assertEquals(RM, e.header(ACCESS_CONTROL_ALLOW_ORIGIN));
    assertTrue(e.header(ACCESS_CONTROL_ALLOW_METHODS).toString().contains("PUT"));
    assertTrue(e.header(ACCESS_CONTROL_ALLOW_HEADERS).toString().contains("Prefer"));
    assertEquals("3600", e.header(ACCESS_CONTROL_MAX_AGE));
    assertNull(e.header(ACCESS_CONTROL_EXPOSE_HEADERS));
  }

  @Test
  public void preflightFromRejectedOriginIsForbidden() {
    Exchange e = preflight("https://evil.example.com").run(filter(RM));
    assertEquals(403, e.aborted.getStatus());
    assertTrue(e.responseHeaders.isEmpty());
  }

  @Test
  public void disabledWhenNoOriginsConfigured() {
    Exchange e = preflight(RM).run(filter());
    assertNull(e.aborted);
    assertTrue(e.responseHeaders.isEmpty());
  }

  @Test
  public void wildcardEchoesAnyOrigin() {
    Exchange e = get("https://anything.example.org").run(filter("*"));
    assertEquals("https://anything.example.org", e.header(ACCESS_CONTROL_ALLOW_ORIGIN));
  }

  @Test
  public void requestWithoutOriginIsLeftAlone() {
    Exchange e = get(null).run(filter("*"));
    assertTrue(e.responseHeaders.isEmpty());
  }

  @Test
  public void sanitizeRemovesLineBreaksAndTruncates() {
    assertEquals("a?b", LyoCorsFilter.sanitize("a\nb"));
    assertTrue(LyoCorsFilter.sanitize("x".repeat(500)).length() < 210);
  }
}
