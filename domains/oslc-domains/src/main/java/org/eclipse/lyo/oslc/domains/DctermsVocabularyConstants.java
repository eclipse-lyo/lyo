// Start of user code Copyright
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
// End of user code

package org.eclipse.lyo.oslc.domains;

// spotless:off
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import javax.xml.namespace.QName;
import java.util.concurrent.ConcurrentHashMap;

// Start of user code imports
// End of user code
// spotless:on

public interface DctermsVocabularyConstants {
  // Namespace & prefix (existing constants retained for binary compatibility)
  String DUBLIN_CORE_NAMSPACE = "http://purl.org/dc/terms/";
  String DUBLIN_CORE_NAMSPACE_PREFIX = "dcterms";

  // Property IRIs (String constants work in annotations; no method calls allowed there)
  String TITLE_PROP = DUBLIN_CORE_NAMSPACE + "title";
  String DESCRIPTION_PROP = DUBLIN_CORE_NAMSPACE + "description";
  String IDENTIFIER_PROP = DUBLIN_CORE_NAMSPACE + "identifier";
  String SUBJECT_PROP = DUBLIN_CORE_NAMSPACE + "subject";
  String CREATOR_PROP = DUBLIN_CORE_NAMSPACE + "creator";
  String CONTRIBUTOR_PROP = DUBLIN_CORE_NAMSPACE + "contributor";
  String CREATED_PROP = DUBLIN_CORE_NAMSPACE + "created";
  String MODIFIED_PROP = DUBLIN_CORE_NAMSPACE + "modified";

  /**
   * Lazy QName helpers to avoid eagerly allocating all QNames while providing convenient access.
   * Usage: DctermsVocabularyConstants.QNames.title() / identifier() etc.
   */
  final class QNames {
    private static final ConcurrentHashMap<String, QName> CACHE = new ConcurrentHashMap<>();

    private QNames() {}

    private static QName get(String local) {
      return CACHE.computeIfAbsent(local, l -> new QName(DUBLIN_CORE_NAMSPACE, l));
    }

    public static QName of(String local) { return get(local); }
    public static QName title() { return get("title"); }
    public static QName description() { return get("description"); }
    public static QName identifier() { return get("identifier"); }
    public static QName subject() { return get("subject"); }
    public static QName creator() { return get("creator"); }
    public static QName contributor() { return get("contributor"); }
    public static QName created() { return get("created"); }
    public static QName modified() { return get("modified"); }
  }
}
