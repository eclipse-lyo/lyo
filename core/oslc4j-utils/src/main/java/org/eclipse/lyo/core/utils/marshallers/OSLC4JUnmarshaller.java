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
package org.eclipse.lyo.core.utils.marshallers;

import static org.eclipse.lyo.core.utils.marshallers.MarshallerConstants.MT_N3;
import static org.eclipse.lyo.core.utils.marshallers.MarshallerConstants.MT_N_TRIPLES;
import static org.eclipse.lyo.core.utils.marshallers.MarshallerConstants.MT_RDF_XML;
import static org.eclipse.lyo.core.utils.marshallers.MarshallerConstants.MT_TURTLE;

import jakarta.ws.rs.core.MediaType;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.net.URISyntaxException;
import javax.xml.datatype.DatatypeConfigurationException;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.RDFReaderI;
import org.apache.jena.util.FileUtils;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;

public class OSLC4JUnmarshaller {

  MediaType mediaType = MT_RDF_XML;

  OSLC4JUnmarshaller() {}

  @SuppressWarnings("unchecked")
  public <T> T unmarshal(InputStream inputStream, Class<T> clazz)
      throws IllegalArgumentException,
          SecurityException,
          DatatypeConfigurationException,
          IllegalAccessException,
          InstantiationException,
          InvocationTargetException,
          OslcCoreApplicationException,
          URISyntaxException,
          NoSuchMethodException {
    final Model model = ModelFactory.createDefaultModel();
    final RDFReaderI reader = getReader(model);
    if (reader == null) { // unsupported media type
      return null;
    }

    // Pass the empty string as the base URI. This allows Jena to
    // resolve relative URIs commonly used to in reified statements
    // for OSLC link labels. See this section of the CM specification
    // for an example:
    // https://docs.oasis-open-projects.org/oslc-op/cm/v3.0/errata01/os/change-mgt-spec.html#labels
    reader.read(model, inputStream, "");

    Object[] result = JenaModelHelper.fromJenaModel(model, clazz);

    T ret = null;

    if (result != null && result.length > 0) {
      ret = (T) result[0];
    }

    return ret;
  }

  private RDFReaderI getReader(final Model model) {
    if (mediaType.isCompatible(MT_RDF_XML)
        || mediaType.isCompatible(MediaType.APPLICATION_XML_TYPE)) {
      // TODO: check
      return model.getReader(
          FileUtils.langXML); // Default reader handles both xml and abbreviated xml
    }

    if (mediaType.isCompatible(MT_N_TRIPLES)) {
      return model.getReader(FileUtils.langNTriple);
    }

    if (mediaType.isCompatible(MT_N3)) {
      return model.getReader(FileUtils.langN3);
    }

    if (mediaType.isCompatible(MT_TURTLE)) {
      return model.getReader(FileUtils.langTurtle);
    }

    return null;
  }

  public MediaType getMediaType() {
    return mediaType;
  }

  public void setMediaType(MediaType mediaType) {
    this.mediaType = mediaType;
  }
}
