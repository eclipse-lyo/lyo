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
package org.eclipse.lyo.oslc4j.provider.jena;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.WebApplicationException;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.HttpHeaders;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.MultivaluedMap;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.Response.ResponseBuilder;
import jakarta.ws.rs.ext.Providers;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.annotation.Annotation;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.time.Instant;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RDFFormat;
import org.apache.jena.riot.RDFLanguages;
import org.apache.jena.riot.RDFWriter;
import org.apache.jena.riot.SysRIOT;
import org.eclipse.lyo.oslc4j.core.OSLC4JConstants;
import org.eclipse.lyo.oslc4j.core.OSLC4JUtils;
import org.eclipse.lyo.oslc4j.core.exception.MessageExtractor;
import org.eclipse.lyo.oslc4j.core.model.Error;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;
import org.eclipse.lyo.oslc4j.core.model.ResponseInfo;
import org.eclipse.lyo.oslc4j.core.model.ResponseInfoArray;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.eclipse.lyo.oslc4j.core.model.ServiceProviderCatalog;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Russell Boykin, Alberto Giammaria, Chris Peters, Gianluca Bernardini, Andrew Berezovskyi
 */
public abstract class AbstractOslcRdfXmlProvider {
  private static final Logger log =
      LoggerFactory.getLogger(AbstractOslcRdfXmlProvider.class.getName());

  /**
   * System property {@value} : When "true", always abbreviate RDF/XML, even
   * when asked for application/rdf+xml. Otherwise, abbreviated RDF/XML is
   * only returned when application/xml is requested. Does not affect
   * text/turtle responses.
   *
   * @see RdfXmlAbbreviatedWriter
   */
  @Deprecated
  public static final String OSLC4J_ALWAYS_XML_ABBREV = OSLC4JConstants.LYO_ALWAYS_XML_ABBREV;

  /**
   * System property {@value} : When "true" (default), fail on when reading a
   * property value that is not a legal instance of a datatype. When "false",
   * skip over invalid values in extended properties.
   */
  @Deprecated
  public static final String OSLC4J_STRICT_DATATYPES = OSLC4JConstants.LYO_STRICT_DATATYPES;

  private static final Annotation[] ANNOTATIONS_EMPTY_ARRAY = new Annotation[0];
  private static final Class<Error> CLASS_OSLC_ERROR = Error.class;

  private @Context HttpHeaders httpHeaders; // Available only on the server
  protected @Context HttpServletRequest httpServletRequest; // Available only on the server
  private @Context Providers providers; // Available on both client and server
  private List<Map.Entry<MediaType, Optional<RDFFormat>>> mediaPairs;

  protected AbstractOslcRdfXmlProvider() {
    super();
  }

  protected void writeTo(
      final Object[] objects,
      final MediaType baseMediaType,
      final MultivaluedMap<String, Object> map,
      final OutputStream outputStream,
      final Map<String, Object> properties,
      final String descriptionURI,
      final String responseInfoURI,
      final ResponseInfo<?> responseInfo)
      throws WebApplicationException {
    Optional<RDFFormat> serializationFormat = getSerializationFormat(baseMediaType);

    // This is a special case to handle RDNG GET on a ServiceProvider resource.
    // RDNG can only consume RDF/XML-ABBREV although its sometimes sends Accept=application/rdf+xml.
    // The alwaysXMLAbbrevOnlyProviders system property enables this special case.
    if ((objects != null && objects.length > 0 && objects[0] != null)
        && (objects[0] instanceof ServiceProviderCatalog || objects[0] instanceof ServiceProvider)
        && serializationFormat.isPresent()
        && RDFLanguages.RDFXML.equals(serializationFormat.get().getLang())
        && "true"
            .equals(System.getProperty(OSLC4JConstants.LYO_ALWAYS_XML_ABBREV_ONLY_PROVIDERS))) {
      serializationFormat = Optional.empty();
      log.info("Using RDF/XML-ABBREV for ServiceProvider resources");
    }

    writeObjectsTo(
        objects,
        outputStream,
        properties,
        descriptionURI,
        responseInfoURI,
        responseInfo,
        serializationFormat);
  }

  private void writeObjectsTo(
      final Object[] objects,
      final OutputStream outputStream,
      final Map<String, Object> properties,
      final String descriptionURI,
      final String responseInfoURI,
      final ResponseInfo<?> responseInfo,
      final Optional<RDFFormat> serializationFormat) {
    try {
      Instant start = Instant.now();

      final Model model =
          JenaModelHelper.createJenaModel(
              descriptionURI, responseInfoURI, responseInfo, objects, properties);

      if (serializationFormat.isPresent()) {
        if (RDFLanguages.RDFXML.equals(serializationFormat.get().getLang())) {
          writeXmlDeclaration(outputStream);
        }
        writeRdf(model, serializationFormat.get(), outputStream);
      } else {
        // No RDFFormat represents our custom abbreviated RDF/XML writer; it isn't a RIOT
        // format at all, so it stays on the legacy RDFWriterI API.
        writeXmlDeclaration(outputStream);
        var writer = new RdfXmlAbbreviatedWriter();
        writer.setProperty("showXmlDeclaration", "false");
        writer.write(model, outputStream, null);
      }
      Instant finish = Instant.now();
      log.trace(
          "writeObjectsTo - Execution Duration: {} ms", Duration.between(start, finish).toMillis());
    } catch (final Exception exception) {
      log.warn(MessageExtractor.getMessage("ErrorSerializingResource"));
      throw new IllegalStateException(exception);
    }
  }

  private void writeXmlDeclaration(final OutputStream outputStream) throws IOException {
    // Jena 6.2's showXmlDeclaration=true omits encoding="UTF-8" even for UTF-8 streams.
    // Keep the encoding explicit for clients that otherwise assume ASCII.
    String xmlDeclaration = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
    outputStream.write(xmlDeclaration.getBytes(StandardCharsets.UTF_8));
  }

  /**
   * Writes any RIOT-backed {@link RDFFormat} (Turtle, JSON-LD, plain RDF/XML) via the modern
   * {@link RDFWriter} builder, replacing the legacy, Jena-6-unreliable
   * {@code model.getWriter(String)} and the deprecated {@code RDFXML_Basic} writer.
   * {@link RDFFormat#RDFXML_PLAIN} is Jena's own non-deprecated replacement for the latter.
   * Using a distinct {@link RDFFormat} per media type (rather than {@link Lang}) also lets
   * {@link #buildMediaPairs()} pick streaming/non-pretty variants for performance.
   */
  private void writeRdf(
      final Model model, final RDFFormat format, final OutputStream outputStream) {
    Map<String, Object> writerProperties =
        RDFFormat.RDFXML_PLAIN.equals(format)
            // The plain writer emits XMLLiterals verbatim as rdf:parseType="Literal", even when
            // they are not well-formed XML (e.g. a dcterms:title of "Washer & Wiper"), which
            // produces malformed RDF/XML. Blocking that rule writes them escaped, with
            // rdf:datatype, as the abbreviated writers do.
            ? Map.of("showXmlDeclaration", "false", "blockRules", "parseTypeLiteralPropertyElt")
            : Map.of("showXmlDeclaration", "false");
    RDFWriter.create()
        .source(model)
        .format(format)
        .set(SysRIOT.sysRdfWriterProperties, writerProperties)
        .output(outputStream);
  }

  protected void writeTo(
      final boolean queryResult,
      final Object[] objects,
      final MediaType baseMediaType,
      final MultivaluedMap<String, Object> map,
      final OutputStream outputStream)
      throws WebApplicationException {
    boolean isClientSide = false;

    try {
      httpServletRequest.getMethod();
    } catch (RuntimeException e) {
      isClientSide = true;
    }

    String descriptionURI = null;
    // TODO Andrew@2019-04-18: stop using responseInfoURI nullity to detect Query results
    String responseInfoURI = null;

    if (queryResult && !isClientSide) {
      log.trace("Marshalling response objects as OSLC Query result (server-side only)");

      final String method = httpServletRequest.getMethod();
      if ("GET".equals(method)) {
        descriptionURI = OSLC4JUtils.resolveURI(httpServletRequest, true);
        responseInfoURI = descriptionURI;

        final String queryString = httpServletRequest.getQueryString();
        if ((queryString != null) && (ProviderHelper.isOslcQuery(queryString))) {
          responseInfoURI += "?" + queryString;
        }
      }
    }

    final Optional<RDFFormat> serializationFormat = getSerializationFormat(baseMediaType);

    @SuppressWarnings("unchecked")
    final Map<String, Object> properties =
        isClientSide
            ? null
            : (Map<String, Object>)
                httpServletRequest.getAttribute(OSLC4JConstants.LYO_SELECTED_PROPERTIES);
    final String nextPageURI =
        isClientSide
            ? null
            : (String) httpServletRequest.getAttribute(OSLC4JConstants.LYO_NEXT_PAGE);
    final Integer totalCount =
        isClientSide
            ? null
            : (Integer) httpServletRequest.getAttribute(OSLC4JConstants.LYO_TOTAL_COUNT);

    ResponseInfo<?> responseInfo =
        new ResponseInfoArray<>(null, properties, totalCount, nextPageURI);

    writeObjectsTo(
        objects,
        outputStream,
        properties,
        descriptionURI,
        responseInfoURI,
        responseInfo,
        serializationFormat);
  }

  /**
   * Determine the {@link RDFFormat} to serialize with based upon mediatype, or
   * {@link Optional#empty()} if the custom abbreviated RDF/XML writer should be used instead
   * (it isn't a RIOT format, so it can't be represented as an {@link RDFFormat}).
   * <p>
   * application/rdf+xml yields plain RDF/XML by default (Jazz compatibility)
   * <p>
   * application/xml yields abbreviated RDF/XML
   */
  private Optional<RDFFormat> getSerializationFormat(final MediaType baseMediaType) {
    if (baseMediaType == null) {
      throw new IllegalArgumentException("Base media type can't be null");
    }

    if (mediaPairs == null) {
      buildMediaPairs();
    }

    for (Map.Entry<MediaType, Optional<RDFFormat>> mediaPair : mediaPairs) {
      if (baseMediaType.isCompatible(mediaPair.getKey())) {
        log.trace(
            "Using '{}' writer for '{}' Accept media type",
            mediaPair.getValue(),
            mediaPair.getKey());
        return mediaPair.getValue();
      }
    }

    throw new IllegalArgumentException("Base media type can't be matched to any writer");
  }

  private void buildMediaPairs() {
    mediaPairs = new ArrayList<>();
    boolean prettyPrint =
        Boolean.parseBoolean(
            System.getProperty(
                OSLC4JConstants.LYO_PRETTY_PRINT, OSLC4JConstants.LYO_PRETTY_PRINT_DEFAULT));
    boolean jazzCompatibility =
        Boolean.parseBoolean(
            System.getProperty(
                OSLC4JConstants.LYO_JAZZ_COMPATIBILITY,
                OSLC4JConstants.LYO_JAZZ_COMPATIBILITY_DEFAULT));
    RDFFormat rdfXmlFormat =
        prettyPrint && !jazzCompatibility ? RDFFormat.RDFXML_PRETTY : RDFFormat.RDFXML_PLAIN;
    mediaPairs.add(
        new AbstractMap.SimpleEntry<>(
            OslcMediaType.TEXT_TURTLE_TYPE,
            Optional.of(prettyPrint ? RDFFormat.TURTLE_PRETTY : RDFFormat.TURTLE_BLOCKS)));
    mediaPairs.add(
        new AbstractMap.SimpleEntry<>(
            OslcMediaType.APPLICATION_JSON_LD_TYPE,
            Optional.of(prettyPrint ? RDFFormat.JSONLD_PRETTY : RDFFormat.JSONLD_FLAT)));
    if (OSLC4JUtils.alwaysAbbrevXML()) {
      // application/rdf+xml will be forcefully abbreviated
      mediaPairs.add(
          new AbstractMap.SimpleEntry<>(OslcMediaType.APPLICATION_RDF_XML_TYPE, Optional.empty()));
    } else {
      mediaPairs.add(
          new AbstractMap.SimpleEntry<>(
              OslcMediaType.APPLICATION_RDF_XML_TYPE, Optional.of(rdfXmlFormat)));
    }
    // application/xml will be abbreviated for compat reasons
    mediaPairs.add(
        new AbstractMap.SimpleEntry<>(OslcMediaType.APPLICATION_XML_TYPE, Optional.empty()));

    mediaPairs.add(
        new AbstractMap.SimpleEntry<>(OslcMediaType.TEXT_XML_TYPE, Optional.of(rdfXmlFormat)));
  }

  protected Object[] readFrom(
      final Class<?> type,
      final MediaType mediaType,
      final MultivaluedMap<String, String> map,
      final InputStream inputStream)
      throws WebApplicationException {
    final Model model = ModelFactory.createDefaultModel();

    //		RDFReaderI reader = getRdfReader(mediaType, model);

    try {
      // Pass the empty string as the base URI. This allows Jena to
      // resolve relative URIs commonly used to in reified statements
      // for OSLC link labels. See this section of the CM specification
      // for an example:
      // https://docs.oasis-open-projects.org/oslc-op/cm/v3.0/errata01/os/change-mgt-spec.html#labels

      var jenaSafeType = mapMimeToSafeJena(mediaType);
      Lang jenaLang = RDFLanguages.contentTypeToLang(jenaSafeType);
      if (jenaLang == null) {
        jenaLang = Lang.RDFXML;
      }

      // Only buffer the stream when trace logging is enabled, so we can log the
      // payload without paying the cost of an extra copy in production.
      final InputStream rdfInputStream;
      if (log.isTraceEnabled()) {
        byte[] data = inputStream.readAllBytes();
        log.trace(
            "readFrom payload ({}, {} bytes):\n{}",
            jenaLang.getName(),
            data.length,
            new String(data, StandardCharsets.UTF_8));
        rdfInputStream = new ByteArrayInputStream(data);
      } else {
        rdfInputStream = inputStream;
      }

      RDFDataMgr.read(model, rdfInputStream, "", jenaLang);
      //			reader.read(model, inputStream, "");

      return JenaModelHelper.unmarshal(model, type);
    } catch (final Exception exception) {
      throw new WebApplicationException(
          exception, buildBadRequestResponse(exception, mediaType, map));
    }
  }

  private String mapMimeToSafeJena(MediaType mediaType) {
    if (mediaType == null) {
      throw new IllegalArgumentException("MIME type must be provided");
    }
    var mimeString = (mediaType.getType() + "/" + mediaType.getSubtype()).toLowerCase();
    if (mimeString.equals("application/xml")) {
      return "application/rdf+xml";
    }
    return mimeString;
  }

  protected Response buildBadRequestResponse(
      final Exception exception,
      final MediaType initialErrorMediaType,
      final MultivaluedMap<String, ?> map) {
    final MediaType determinedErrorMediaType = determineErrorMediaType(initialErrorMediaType, map);

    final Error error = new Error();

    error.setStatusCode(String.valueOf(Response.Status.BAD_REQUEST.getStatusCode()));
    error.setMessage(exception.getMessage());

    final ResponseBuilder responseBuilder = Response.status(Response.Status.BAD_REQUEST);
    return responseBuilder.type(determinedErrorMediaType).entity(error).build();
  }

  /**
   * We handle the case where a client requests an accept type different than their content type.
   * This will work correctly in the successful case.	 But, in the error case where our
   * MessageBodyReader/MessageBodyWriter fails internally, we respect the client's requested accept type.
   */
  private MediaType determineErrorMediaType(
      final MediaType initialErrorMediaType, final MultivaluedMap<String, ?> map) {
    try {
      // HttpHeaders will not be available on the client side and will throw a NullPointerException
      final List<MediaType> acceptableMediaTypes = httpHeaders.getAcceptableMediaTypes();

      // Since we got here, we know we are executing on the server

      if (acceptableMediaTypes != null) {
        for (final MediaType acceptableMediaType : acceptableMediaTypes) {
          // If a concrete media type with a MessageBodyWriter
          if (isAcceptableMediaType(acceptableMediaType)) {
            return acceptableMediaType;
          }
        }
      }
    } catch (final NullPointerException exception) {
      // Ignore NullPointerException since this means the context has not been set since we are on
      // the client

      // See if the MultivaluedMap for headers is available
      if (map != null) {
        final Object acceptObject = map.getFirst("Accept");

        if (acceptObject instanceof String) {
          final String[] accepts = acceptObject.toString().split(",");

          double winningQ = 0.0D;
          MediaType winningMediaType = null;

          for (final String accept : accepts) {
            final MediaType acceptMediaType = MediaType.valueOf(accept);

            // If a concrete media type with a MessageBodyWriter
            if (isAcceptableMediaType(acceptMediaType)) {
              final String stringQ = acceptMediaType.getParameters().get("q");

              final double q = stringQ == null ? 1.0D : Double.parseDouble(stringQ);

              // Make sure and exclude blackballed media type
              if (Double.compare(q, 0.0D) > 0) {
                if ((winningMediaType == null) || (Double.compare(q, winningQ) > 0)) {
                  winningMediaType = acceptMediaType;
                  winningQ = q;
                }
              }
            }
          }

          if (winningMediaType != null) {
            return winningMediaType;
          }
        }
      }
    }

    return initialErrorMediaType;
  }

  /**
   * Only allow media types that are not wildcards and have a related MessageBodyWriter for OSLC Error.
   */
  private boolean isAcceptableMediaType(final MediaType mediaType) {
    return (!mediaType.isWildcardType())
        && (!mediaType.isWildcardSubtype())
        && (providers.getMessageBodyWriter(
                CLASS_OSLC_ERROR, CLASS_OSLC_ERROR, ANNOTATIONS_EMPTY_ARRAY, mediaType)
            != null);
  }
}
