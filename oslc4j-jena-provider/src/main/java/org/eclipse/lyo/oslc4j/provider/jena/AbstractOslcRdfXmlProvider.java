/*!*****************************************************************************
 * Copyright (c) 2012 - 2018 IBM Corporation and others.
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
 *	   Russell Boykin		- initial API and implementation
 *	   Alberto Giammaria	- initial API and implementation
 *	   Chris Peters			- initial API and implementation
 *	   Gianluca Bernardini	- initial API and implementation
 *	   Andrew Berezovskyi   - JSON-LD support
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.provider.jena;

import java.io.InputStream;
import java.io.OutputStream;
import java.lang.annotation.Annotation;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.ResponseBuilder;
import javax.ws.rs.ext.Providers;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.RDFReader;
import org.apache.jena.rdf.model.RDFWriter;
import org.apache.jena.riot.RDFLanguages;
import org.apache.jena.util.FileUtils;
import org.eclipse.lyo.oslc4j.core.OSLC4JConstants;
import org.eclipse.lyo.oslc4j.core.OSLC4JUtils;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNotQueryResult;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.exception.MessageExtractor;
import org.eclipse.lyo.oslc4j.core.model.Error;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;
import org.eclipse.lyo.oslc4j.core.model.ResponseInfo;
import org.eclipse.lyo.oslc4j.core.model.ResponseInfoArray;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.eclipse.lyo.oslc4j.core.model.ServiceProviderCatalog;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class AbstractOslcRdfXmlProvider
{
	private static final Logger log = LoggerFactory.getLogger(AbstractOslcRdfXmlProvider.class.getName
			());

	/**
	 * System property {@value} : When "true", always abbreviate RDF/XML, even
	 * when asked for application/rdf+xml. Otherwise, abbreviated RDF/XML is
	 * only returned when application/xml is requested. Does not affect
	 * text/turtle responses.
	 *
	 * @see RdfXmlAbbreviatedWriter
	 */
	@Deprecated
	public static final String OSLC4J_ALWAYS_XML_ABBREV		  = "org.eclipse.lyo.oslc4j.alwaysXMLAbbrev";

	/**
	 * System property {@value} : When "true" (default), fail on when reading a
	 * property value that is not a legal instance of a datatype. When "false",
	 * skip over invalid values in extended properties.
	 */
	@Deprecated
	public static final String OSLC4J_STRICT_DATATYPES		 = "org.eclipse.lyo.oslc4j.strictDatatypes";

	private static final Annotation[] ANNOTATIONS_EMPTY_ARRAY = new Annotation[0];
	private static final Class<Error> CLASS_OSLC_ERROR		  = Error.class;
	private static final ErrorHandler ERROR_HANDLER			  = new ErrorHandler();

	private @Context HttpHeaders		  httpHeaders;		  // Available only on the server
	protected @Context HttpServletRequest httpServletRequest; // Available only on the server
	private @Context Providers			  providers;		  // Available on both client and server

	protected AbstractOslcRdfXmlProvider()
	{
		super();
	}

	protected static boolean isWriteable(final Class<?>		 type,
										 final Annotation[]	 annotations,
										 final MediaType	 actualMediaType,
										 final MediaType ... requiredMediaTypes)
	{
		return true;
	}

	protected void writeTo(final Object[]						objects,
						   final MediaType						baseMediaType,
						   final MultivaluedMap<String, Object> map,
						   final OutputStream					outputStream,
						   final Map<String, Object>			properties,
						   final String							descriptionURI,
						   final String							responseInfoURI,
						   final ResponseInfo<?>				responseInfo)
				throws WebApplicationException
	{
		String serializationLanguage = getSerializationLanguage(baseMediaType);

		// This is a special case to handle RDNG GET on a ServiceProvider resource.
		// RDNG can only consume RDF/XML-ABBREV although its sometimes sends Accept=application/rdf+xml.
		// The org.eclipse.lyo.oslc4j.alwaysXMLAbbrevOnlyProviders system property is used
		// to turn off this special case
		if ((objects != null && objects[0] != null) &&
			( objects[0] instanceof ServiceProviderCatalog || objects[0] instanceof ServiceProvider ) &&
			serializationLanguage.equals(FileUtils.langXML) &&
			"true".equals(System.getProperty("org.eclipse.lyo.oslc4j.alwaysXMLAbbrevOnlyProviders"))) {
			serializationLanguage = FileUtils.langXMLAbbrev;
			log.info("Using RDF/XML-ABBREV for ServiceProvider resources");
		}

		writeObjectsTo(objects,
					   outputStream,
					   properties,
					   descriptionURI,
					   responseInfoURI,
					   responseInfo,
					   serializationLanguage
		);
	}

	private void writeObjectsTo(final Object[] objects, final OutputStream outputStream,
			final Map<String, Object> properties, final String descriptionURI,
			final String responseInfoURI, final ResponseInfo<?> responseInfo,
			final String serializationLanguage) {
		try
		{
			final Model model = JenaModelHelper.createJenaModel(descriptionURI,
																responseInfoURI,
																responseInfo,
																objects,
																properties);
			RDFWriter writer = getRdfWriter(serializationLanguage, model);

			if (serializationLanguage.equals(FileUtils.langXML) || serializationLanguage.equals(FileUtils.langXMLAbbrev))
			{
				// XML (and Jena) default to UTF-8, but many libs default to ASCII, so need
				// to set this explicitly
				writer.setProperty("showXmlDeclaration",
								   "false");
				String xmlDeclaration = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
				outputStream.write(xmlDeclaration.getBytes());
			}
			writer.write(model,
						 outputStream,
						 null);
		}
		catch (final Exception exception)
		{
			log.warn(MessageExtractor.getMessage("ErrorSerializingResource"));
			throw new IllegalStateException(exception);
		}
	}

	private RDFWriter getRdfWriter(final String serializationLanguage, final Model model) {
		RDFWriter writer;
		if	(serializationLanguage.equals(FileUtils.langXMLAbbrev))
        {
            writer = new RdfXmlAbbreviatedWriter();
        }
        else
        {
            writer = model.getWriter(serializationLanguage);
        }
		writer.setProperty("showXmlDeclaration",
                           "false");
		writer.setErrorHandler(ERROR_HANDLER);
		return writer;
	}

	protected void writeTo(final boolean						queryResult,
						   final Object[]						objects,
						   final MediaType						baseMediaType,
						   final MultivaluedMap<String, Object> map,
						   final OutputStream					outputStream)
			  throws WebApplicationException
	{
		boolean isClientSide = false;

		try {
			httpServletRequest.getMethod();
		} catch (RuntimeException e) {
			isClientSide = true;
		}

		String descriptionURI  = null;
		// TODO Andrew@2019-04-18: stop using responseInfoURI nullity to detect Query results
		String responseInfoURI = null;

		if (queryResult && ! isClientSide)
		{
			log.trace("Marshalling response objects as OSLC Query result (server-side only)");

			final String method = httpServletRequest.getMethod();
			if ("GET".equals(method))
			{
				descriptionURI =  OSLC4JUtils.resolveURI(httpServletRequest,true);
				responseInfoURI = descriptionURI;

				final String queryString = httpServletRequest.getQueryString();
				if ((queryString != null) &&
					(isOslcQuery(queryString)))
				{
					responseInfoURI += "?" + queryString;
				}
			}

		}

		final String serializationLanguage = getSerializationLanguage(baseMediaType);

		@SuppressWarnings("unchecked")
		final Map<String, Object> properties = isClientSide ?
			null :
			(Map<String, Object>)httpServletRequest.getAttribute(OSLC4JConstants.OSLC4J_SELECTED_PROPERTIES);
		final String nextPageURI = isClientSide ?
			null :
			(String)httpServletRequest.getAttribute(OSLC4JConstants.OSLC4J_NEXT_PAGE);
		final Integer totalCount = isClientSide ?
			null :
			(Integer)httpServletRequest.getAttribute(OSLC4JConstants.OSLC4J_TOTAL_COUNT);

		ResponseInfo<?> responseInfo = new ResponseInfoArray<>(null, properties, totalCount,
				nextPageURI);

		writeObjectsTo(
				objects,
				outputStream,
				properties,
				descriptionURI,
				responseInfoURI,
				responseInfo,
				serializationLanguage
		);
	}

	/**
	 * Determine whether to serialize in xml or abreviated xml based upon mediatype.
	 * <p>
	 * application/rdf+xml yields xml
	 * <p>
	 * applicaton/xml yields abbreviated xml
	 */
	private String getSerializationLanguage(final MediaType baseMediaType) {

		if(baseMediaType == null) {
			throw new IllegalArgumentException("Base media type can't be null");
		}

		List<Map.Entry<MediaType, String>> mediaPairs = new ArrayList<>();
		mediaPairs.add(new AbstractMap.SimpleEntry<>(OslcMediaType.TEXT_TURTLE_TYPE,
													 RDFLanguages.strLangTurtle));
		mediaPairs.add(new AbstractMap.SimpleEntry<>(OslcMediaType.APPLICATION_JSON_LD_TYPE,
													 RDFLanguages.strLangJSONLD));
		if (OSLC4JUtils.alwaysAbbrevXML()) {
			// application/rdf+xml will be forcefully abbreviated
			mediaPairs.add(new AbstractMap.SimpleEntry<>(OslcMediaType.APPLICATION_RDF_XML_TYPE,
														 FileUtils.langXMLAbbrev));
		} else {
			mediaPairs.add(new AbstractMap.SimpleEntry<>(OslcMediaType.APPLICATION_RDF_XML_TYPE,
														 RDFLanguages.strLangRDFXML));

		}
		// application/xml will be abbreviated for compat reasons
		mediaPairs.add(new AbstractMap.SimpleEntry<>(OslcMediaType.APPLICATION_XML_TYPE,
													 FileUtils.langXMLAbbrev));

		mediaPairs.add(new AbstractMap.SimpleEntry<>(OslcMediaType.TEXT_XML_TYPE,
				 RDFLanguages.strLangRDFXML));
		
		for (Map.Entry<MediaType, String> mediaPair : mediaPairs) {
			if (baseMediaType.isCompatible(mediaPair.getKey())) {
				log.trace("Using '{}' writer for '{}' Accept media type",
						  mediaPair.getValue(),
						  mediaPair.getKey());
				return mediaPair.getValue();
			}
		}

		throw new IllegalArgumentException("Base media type can't be matched to any writer");
	}

	protected static boolean isReadable(final Class<?>		type,
										final MediaType		actualMediaType,
										final MediaType ... requiredMediaTypes)
	{
		if (type.getAnnotation(OslcResourceShape.class) != null)
		{
			return isCompatible(actualMediaType, requiredMediaTypes);
		}

		return false;
	}

	protected static boolean isCompatible(final MediaType actualMediaType,
			final MediaType... requiredMediaTypes)
	{
			for (final MediaType requiredMediaType : requiredMediaTypes)
			{
				if (requiredMediaType.isCompatible(actualMediaType))
				{
					return true;
				}
			}

		return false;
	}

	protected Object[] readFrom(final Class<?>						 type,
								final MediaType						 mediaType,
								final MultivaluedMap<String, String> map,
								final InputStream					 inputStream)
			  throws WebApplicationException
	{
		final Model model = ModelFactory.createDefaultModel();

		RDFReader reader = getRdfReader(mediaType, model);

		try
		{
			// Pass the empty string as the base URI. This allows Jena to
			// resolve relative URIs commonly used to in reified statements
			// for OSLC link labels. See this section of the CM specification
			// for an example:
			// http://open-services.net/bin/view/Main/CmSpecificationV2?sortcol=table;up=#Labels_for_Relationships

			reader.read(model, inputStream, "");

			return JenaModelHelper.unmarshal(model, type);
		}
		catch (final Exception exception)
		{
			throw new WebApplicationException(exception,
											  buildBadRequestResponse(exception,
																	  mediaType,
																	  map));
		}
	}

	private RDFReader getRdfReader(final MediaType mediaType, final Model model) {
		RDFReader reader;
		final String language = getSerializationLanguage(mediaType);
		if (language.equals(FileUtils.langXMLAbbrev))
		{
			reader = model.getReader(); // Default reader handles both xml and abbreviated xml
		} else {
			reader=model.getReader(language);
		}
		reader.setErrorHandler(ERROR_HANDLER);
		return reader;
	}

	protected Response buildBadRequestResponse(final Exception				   exception,
											   final MediaType				   initialErrorMediaType,
											   final MultivaluedMap<String, ?> map)
	{
		final MediaType determinedErrorMediaType = determineErrorMediaType(initialErrorMediaType,
																		   map);

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
	private MediaType determineErrorMediaType(final MediaType				  initialErrorMediaType,
											  final MultivaluedMap<String, ?> map)
	{
		try
		{
			// HttpHeaders will not be available on the client side and will throw a NullPointerException
			final List<MediaType> acceptableMediaTypes = httpHeaders.getAcceptableMediaTypes();

			// Since we got here, we know we are executing on the server

			if (acceptableMediaTypes != null)
			{
				for (final MediaType acceptableMediaType : acceptableMediaTypes)
				{
					// If a concrete media type with a MessageBodyWriter
					if (isAcceptableMediaType(acceptableMediaType))
					{
						return acceptableMediaType;
					}
				}
			}
		}
		catch (final NullPointerException exception)
		{
			// Ignore NullPointerException since this means the context has not been set since we are on the client

			// See if the MultivaluedMap for headers is available
			if (map != null)
			{
				final Object acceptObject = map.getFirst("Accept");

				if (acceptObject instanceof String)
				{
					final String[] accepts = acceptObject.toString().split(",");

					double	  winningQ		   = 0.0D;
					MediaType winningMediaType = null;

					for (final String accept : accepts)
					{
						final MediaType acceptMediaType = MediaType.valueOf(accept);

						// If a concrete media type with a MessageBodyWriter
						if (isAcceptableMediaType(acceptMediaType))
						{
							final String stringQ = acceptMediaType.getParameters().get("q");

							final double q = stringQ == null ? 1.0D : Double.parseDouble(stringQ);

							// Make sure and exclude blackballed media type
							if (Double.compare(q, 0.0D) > 0)
							{
								if ((winningMediaType == null) ||
									(Double.compare(q, winningQ) > 0))
								{
									winningMediaType = acceptMediaType;
									winningQ		 = q;
								}
							}
						}
					}

					if (winningMediaType != null)
					{
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
	private boolean isAcceptableMediaType(final MediaType mediaType)
	{
		return (!mediaType.isWildcardType()) &&
			   (!mediaType.isWildcardSubtype()) &&
			   (providers.getMessageBodyWriter(CLASS_OSLC_ERROR,
											   CLASS_OSLC_ERROR,
											   ANNOTATIONS_EMPTY_ARRAY,
											   mediaType) != null);
	}

	protected static boolean isOslcQuery(final String parmString)
	{
		boolean containsOslcParm = false;

		final String [] uriParts = parmString.toLowerCase().split("oslc\\.",2);
		if (uriParts.length > 1)
		{
			containsOslcParm = true;
		}

		return containsOslcParm;
	}
}
