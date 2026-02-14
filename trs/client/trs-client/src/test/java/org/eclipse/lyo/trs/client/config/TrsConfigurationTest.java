/*
 * Copyright (c) 2025 Contributors to the Eclipse Foundation
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
package org.eclipse.lyo.trs.client.config;

import static org.junit.jupiter.api.Assertions.*;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URI;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

@DisplayName("TRS Configuration Tests")
public class TrsConfigurationTest {

  @TempDir File tempDir;

  @Test
  @DisplayName("Test TrsConfigurationLoader handles missing trs_uri")
  public void testTrsConfigurationLoaderMissingTrsUri() throws IOException {
    // Create a properties file without trs_uri
    File configFile = new File(tempDir, "test.properties");
    try (FileWriter writer = new FileWriter(configFile)) {
      writer.write("baseAuth_user=testuser\n");
      writer.write("baseAuth_password=testpass\n");
    }

    // Loading should throw IllegalStateException
    IllegalStateException exception =
        assertThrows(
            IllegalStateException.class,
            () -> {
              TrsConfigurationLoader.from(configFile);
            });

    assertTrue(exception.getMessage().contains("trs_uri"));
    assertTrue(exception.getMessage().contains("missing"));
  }

  @Test
  @DisplayName("Test TrsConfigurationLoader with valid configuration")
  public void testTrsConfigurationLoaderValid() throws IOException {
    // Create a valid properties file
    File configFile = new File(tempDir, "test.properties");
    try (FileWriter writer = new FileWriter(configFile)) {
      writer.write("trs_uri=http://example.org/trs\n");
      writer.write("baseAuth_user=testuser\n");
      writer.write("baseAuth_pwd=testpass\n");
      writer.write("sparql_query_url=http://example.org/sparql/query\n");
      writer.write("sparql_update_url=http://example.org/sparql/update\n");
      writer.write("sparql_username=sparqluser\n");
      writer.write("sparql_password=sparqlpass\n");
    }

    // Loading should succeed
    TrsProviderConfiguration config = TrsConfigurationLoader.from(configFile);

    assertNotNull(config);
    assertEquals(URI.create("http://example.org/trs"), config.getTrsUri());
    assertEquals("testuser", config.getBasicAuthUsername());
    assertEquals("testpass", config.getBasicAuthPassword());
//    assertEquals("http://example.org/sparql/query", config.getSparqlQueryUrl());
//    assertEquals("http://example.org/sparql/update", config.getSparqlUpdateUrl());
//    assertEquals("sparqluser", config.getSparqlUsername());
//    assertEquals("sparqlpass", config.getSparqlPassword());
  }

  @Test
  @DisplayName("Test TrsConfigurationLoader with minimal configuration")
  public void testTrsConfigurationLoaderMinimal() throws IOException {
    // Create a minimal properties file with only required trs_uri
    File configFile = new File(tempDir, "test.properties");
    try (FileWriter writer = new FileWriter(configFile)) {
      writer.write("trs_uri=http://example.org/trs\n");
    }

    // Loading should succeed
    TrsProviderConfiguration config = TrsConfigurationLoader.from(configFile);

    assertNotNull(config);
    assertEquals(URI.create("http://example.org/trs"), config.getTrsUri());
    assertNull(config.getBasicAuthUsername());
    assertNull(config.getBasicAuthPassword());
//    assertNull(config.getSparqlQueryUrl());
//    assertNull(config.getSparqlUpdateUrl());
//    assertNull(config.getSparqlUsername());
//    assertNull(config.getSparqlPassword());
  }

  @Test
  @DisplayName("Test TrsConsumerConfiguration with null basic auth")
  public void testTrsConsumerConfigurationNullAuth() {
    TrsConsumerConfiguration config =
        new TrsConsumerConfiguration(
            "http://example.org/sparql/query",
            "http://example.org/sparql/update",
            "sparqluser",
            "sparqlpass",
            null, // scheduler
            null, // basicUsername
            null // basicPassword
            );

    // getHttpClient should handle null basic auth gracefully
    assertNotNull(config.getHttpClient());
  }

  @Test
  @DisplayName("Test TrsConsumerConfiguration with empty basic auth")
  public void testTrsConsumerConfigurationEmptyAuth() {
    TrsConsumerConfiguration config =
        new TrsConsumerConfiguration(
            "http://example.org/sparql/query",
            "http://example.org/sparql/update",
            "sparqluser",
            "sparqlpass",
            null, // scheduler
            "", // empty basicUsername
            "" // empty basicPassword
            );

    // getHttpClient should handle empty basic auth gracefully
    assertNotNull(config.getHttpClient());
  }

  @Test
  @DisplayName("Test TrsConsumerConfiguration with valid basic auth")
  public void testTrsConsumerConfigurationValidAuth() {
    TrsConsumerConfiguration config =
        new TrsConsumerConfiguration(
            "http://example.org/sparql/query",
            "http://example.org/sparql/update",
            "sparqluser",
            "sparqlpass",
            null, // scheduler
            "basicuser",
            "basicpass");

    // getHttpClient should configure basic auth
    assertNotNull(config.getHttpClient());
  }
}
