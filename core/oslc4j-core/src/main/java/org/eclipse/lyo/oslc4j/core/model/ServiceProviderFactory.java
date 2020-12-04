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
package org.eclipse.lyo.oslc4j.core.model;

import java.io.UnsupportedEncodingException;
import java.lang.reflect.Method;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLEncoder;
import java.util.HashMap;
import java.util.Map;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.core.UriBuilder;
import org.eclipse.lyo.oslc4j.core.annotation.OslcCreationFactory;
import org.eclipse.lyo.oslc4j.core.annotation.OslcDialog;
import org.eclipse.lyo.oslc4j.core.annotation.OslcDialogs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcQueryCapability;
import org.eclipse.lyo.oslc4j.core.annotation.OslcService;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreMissingAnnotationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class ServiceProviderFactory {
	private final static Logger log = LoggerFactory.getLogger(ServiceProviderFactory.class);

	private ServiceProviderFactory() {
		super();
	}

	public static ServiceProvider createServiceProvider(final String baseURI, final String genericBaseURI, final String title, final String description, final Publisher publisher, final Class<?>[] resourceClasses) throws OslcCoreApplicationException, URISyntaxException {
		return initServiceProvider(new ServiceProvider(), baseURI, genericBaseURI, title, description, publisher, resourceClasses, null);
	}
	
	public static ServiceProvider createServiceProvider(final String baseURI, final String genericBaseURI, final String title, final String description, final Publisher publisher, final Class<?>[] resourceClasses, final Map<String,Object> pathParameterValues) throws OslcCoreApplicationException, URISyntaxException {
		return initServiceProvider(new ServiceProvider(), baseURI, genericBaseURI, title, description, publisher, resourceClasses, pathParameterValues);
	}

	public static ServiceProvider initServiceProvider(final ServiceProvider serviceProvider, final String baseURI, final String genericBaseURI, final String title, final String description, final Publisher publisher, final Class<?>[] resourceClasses, final Map<String,Object> pathParameterValues) throws OslcCoreApplicationException, URISyntaxException {
		serviceProvider.setTitle(title);
		serviceProvider.setDescription(description);
		serviceProvider.setPublisher(publisher);

		final Map<String, Service> serviceMap = new HashMap<String, Service>();

		for (final Class<?> resourceClass : resourceClasses) {
			final OslcService serviceAnnotation = resourceClass.getAnnotation(OslcService.class);

			if (serviceAnnotation == null) {
				throw new OslcCoreMissingAnnotationException(resourceClass, OslcService.class);
			}

			final String domain = serviceAnnotation.value();

			Service service = serviceMap.get(domain);
			if (service == null) {
				service = new Service(new URI(domain));
				serviceMap.put(domain, service);
			}

			Map<String, Object> initPathParameterValues = pathParameterValues;
			if (initPathParameterValues == null) {
				log.warn("pathParameterValues passed to ServiceProviderFactory.initServiceProvider() SHALL NOT be null");
				initPathParameterValues = new HashMap<>();
			}
			handleResourceClass(baseURI, genericBaseURI, resourceClass, service, initPathParameterValues);
		}

		// add the services to the provider
		for (final Service service : serviceMap.values()) {
			serviceProvider.addService(service);
		}

		return serviceProvider;
	}

	private static void handleResourceClass(final String baseURI, final String genericBaseURI, final Class<?> resourceClass,
			final Service service, final Map<String,Object> pathParameterValues) throws URISyntaxException {
		for (final Method method : resourceClass.getMethods()) {
			final GET getAnnotation = method.getAnnotation(GET.class);
			if (getAnnotation != null) {
				final OslcQueryCapability queryCapabilityAnnotation = method.getAnnotation(OslcQueryCapability.class);
				String[] resourceShapes = null;
				if (queryCapabilityAnnotation != null) {
					service.addQueryCapability(createQueryCapability(baseURI, method, pathParameterValues));
					final String resourceShape = queryCapabilityAnnotation.resourceShape();
					if ((resourceShape != null) && (resourceShape.length() > 0)) {
						resourceShapes = new String[] {resourceShape};
					}
				}
				final OslcDialogs dialogsAnnotation = method.getAnnotation(OslcDialogs.class);
				if (dialogsAnnotation != null) {
					final OslcDialog[] dialogs = dialogsAnnotation.value();
					for (final OslcDialog dialog : dialogs) {
						if (dialog != null) {
							service.addSelectionDialog(createSelectionDialog(baseURI, genericBaseURI, method, dialog, resourceShapes, pathParameterValues));
						}
					}
				}
				else
				{
					final OslcDialog dialogAnnotation = method.getAnnotation(OslcDialog.class);
					if (dialogAnnotation != null) {
						service.addSelectionDialog(createSelectionDialog(baseURI, genericBaseURI, method, dialogAnnotation, resourceShapes, pathParameterValues));
					}
				}
			} else {
				final POST postAnnotation = method.getAnnotation(POST.class);
				if (postAnnotation != null) {
					final OslcCreationFactory creationFactoryAnnotation = method.getAnnotation(OslcCreationFactory.class);
					String[] resourceShapes = null;
					if (creationFactoryAnnotation != null) {
						service.addCreationFactory(createCreationFactory(baseURI,
								pathParameterValues, method));
						resourceShapes = creationFactoryAnnotation.resourceShapes();
					}
					final OslcDialogs dialogsAnnotation = method.getAnnotation(OslcDialogs.class);
					if (dialogsAnnotation != null) {
						final OslcDialog[] dialogs = dialogsAnnotation.value();
						for (final OslcDialog dialog : dialogs) {
							if (dialog != null) {
								service.addCreationDialog(createCreationDialog(baseURI, genericBaseURI, method, dialog, resourceShapes, pathParameterValues));
							}
						}
					}
					else
					{
						final OslcDialog dialogAnnotation = method.getAnnotation(OslcDialog.class);
						if (dialogAnnotation != null) {
							service.addCreationDialog(createCreationDialog(baseURI, genericBaseURI, method, dialogAnnotation, resourceShapes, pathParameterValues));
						}
					}
				}
			}
		}
	}

	static CreationFactory createCreationFactory(final String baseURI, final Map<String, Object> pathParameterValues,
			final Method method)
			throws URISyntaxException {
		final Path classPathAnnotation = method.getDeclaringClass().getAnnotation(Path.class);
		final OslcCreationFactory creationFactoryAnnotation = method.getAnnotation(
				OslcCreationFactory.class);
		final Path methodPathAnnotation = method.getAnnotation(Path.class);

		CreationFactory creationFactory = createCreationFactory(baseURI, pathParameterValues,
				classPathAnnotation, creationFactoryAnnotation, methodPathAnnotation);

		return creationFactory;
	}

	static CreationFactory createCreationFactory(final String baseURI, final Map<String, Object> pathParameterValues,
			final Path classPathAnnotation, final OslcCreationFactory creationFactoryAnnotation,
			final Path methodPathAnnotation)
			throws URISyntaxException {
		final String title = creationFactoryAnnotation.title();
		final String label = creationFactoryAnnotation.label();
		final String[] resourceShapes = creationFactoryAnnotation.resourceShapes();
		final String[] resourceTypes = creationFactoryAnnotation.resourceTypes();
		final String[] usages = creationFactoryAnnotation.usages();

		final String basePath = baseURI + "/";

		String creation = resolvePathParameters(basePath,
				pathAnnotationStringValue(classPathAnnotation),
				pathAnnotationStringValue(methodPathAnnotation), pathParameterValues);

		CreationFactory creationFactory = new CreationFactory(title, new URI(creation).normalize());

		if ((label != null) && (label.length() > 0)) {
			creationFactory.setLabel(label);
		}

		for (final String resourceShape : resourceShapes) {
			creationFactory.addResourceShape(new URI(basePath + resourceShape).normalize());
		}

		for (final String resourceType : resourceTypes) {
			creationFactory.addResourceType(new URI(resourceType));
		}

		for (final String usage : usages) {
			creationFactory.addUsage(new URI(usage));
		}
		return creationFactory;
	}

	private static String pathAnnotationStringValue(final Path pathAnnotation) {
		return pathAnnotation == null ? null : pathAnnotation.value();
	}

	private static QueryCapability createQueryCapability(final String baseURI, final Method method, final Map<String,Object> pathParameterValues) throws URISyntaxException {
		final OslcQueryCapability queryCapabilityAnnotation = method.getAnnotation(OslcQueryCapability.class);

		final String title = queryCapabilityAnnotation.title();
		final String label = queryCapabilityAnnotation.label();
		final String resourceShape = queryCapabilityAnnotation.resourceShape();
		final String[] resourceTypes = queryCapabilityAnnotation.resourceTypes();
		final String[] usages = queryCapabilityAnnotation.usages();

		final String basePath = baseURI + "/";

		final Path classPathAnnotation  = method.getDeclaringClass().getAnnotation(Path.class);
		final Path methodPathAnnotation = method.getAnnotation(Path.class);

		String query = resolvePathParameters(basePath,
				(pathAnnotationStringValue(classPathAnnotation)),
				(pathAnnotationStringValue(methodPathAnnotation)), pathParameterValues);

		QueryCapability queryCapability = new QueryCapability(title, new URI(query).normalize());

		if ((label != null) && (label.length() > 0)) {
			queryCapability.setLabel(label);
		}

		if ((resourceShape != null) && (resourceShape.length() > 0)) {
			queryCapability.setResourceShape(new URI(basePath + resourceShape).normalize());
		}

		for (final String resourceType : resourceTypes) {
			queryCapability.addResourceType(new URI(resourceType));
		}

		for (final String usage : usages) {
			queryCapability.addUsage(new URI(usage));
		}

		return queryCapability;
	}

	private static Dialog createCreationDialog(final String baseURI, final String genericBaseURI, final Method method, final OslcDialog dialogAnnotation, final String[] resourceShapes,
			final Map<String,Object> pathParameterValues) throws URISyntaxException {
		return createDialog(baseURI, genericBaseURI, "Creation", "creation", method, dialogAnnotation, resourceShapes, pathParameterValues);
	}

	private static Dialog createSelectionDialog(final String baseURI, final String genericBaseURI, final Method method, final OslcDialog dialogAnnotation, final String[] resourceShapes,
			final Map<String,Object> pathParameterValues) throws URISyntaxException {
		return createDialog(baseURI, genericBaseURI, "Selection", "queryBase", method, dialogAnnotation, resourceShapes, pathParameterValues);
	}

	private static Dialog createDialog(final String baseURI, final String genericBaseURI, final String dialogType, final String parameterName, final Method method, final OslcDialog dialogAnnotation, final String[] resourceShapes,
			final Map<String,Object> pathParameterValues) throws URISyntaxException {

		final String title = dialogAnnotation.title();
		final String label = dialogAnnotation.label();
		final String dialogURI = dialogAnnotation.uri();
		final String hintWidth = dialogAnnotation.hintWidth();
		final String hintHeight = dialogAnnotation.hintHeight();
		final String[] resourceTypes = dialogAnnotation.resourceTypes();
		final String[] usages = dialogAnnotation.usages();

		String uri = "";

		final Path classPathAnnotation = method.getDeclaringClass().getAnnotation(Path.class);

		if (dialogURI.length() > 0)
		{
			// TODO: Do we chop off everything after the port and then append the dialog URI?
			//		 For now just assume that the dialog URI builds on top of the baseURI.
			uri = resolvePathParameters(baseURI, null, dialogURI, pathParameterValues);
		}
		else
		{
			uri = genericBaseURI + "/generic/generic" + dialogType + ".html";


			final Path methodPathAnnotation = method.getAnnotation(Path.class);

			String parameter = resolvePathParameters(baseURI,
					(pathAnnotationStringValue(classPathAnnotation)),
					(pathAnnotationStringValue(methodPathAnnotation)),
					pathParameterValues);

			try {
				final String encodedParameter = URLEncoder.encode(parameter, "UTF-8");
				uri += "?" + parameterName + "=" + encodedParameter;
			} catch (final UnsupportedEncodingException exception) {
				// TODO Andrew@2017-07-18: Rethrow an exception
				log.warn("Error encoding URI [{}]", parameter, exception);
			}

			StringBuilder resourceShapeParameters = new StringBuilder();

			if (resourceShapes != null) {
				for (final String resourceShape : resourceShapes) {
					final String resourceShapeURI = baseURI + '/' + resourceShape;

					try {
						final String encodedResourceShape = URLEncoder.encode(resourceShapeURI,
								"UTF-8");

						resourceShapeParameters.append("&resourceShape=").append(
								encodedResourceShape);
					} catch (final UnsupportedEncodingException exception) {
						// TODO Andrew@2017-07-18: Rethrow an exception
						log.warn("Error encoding URI [{}]", resourceShapeURI, exception);
					}
				}
			}

			uri += resourceShapeParameters.toString();
		}

		Dialog dialog = new Dialog(title, new URI(uri).normalize());

		if ((label != null) && (label.length() > 0)) {
			dialog.setLabel(label);
		}

		if ((hintWidth != null) && (hintWidth.length() > 0)) {
			dialog.setHintWidth(hintWidth);
		}

		if ((hintHeight != null) && (hintHeight.length() > 0)) {
			dialog.setHintHeight(hintHeight);
		}

		for (final String resourceType : resourceTypes) {
			dialog.addResourceType(new URI(resourceType));
		}

		for (final String usage : usages) {
			dialog.addUsage(new URI(usage));
		}

		return dialog;
	}

	/**
	 * Build the path from the @Path template + map of parameter value replacements
	 *
	 */
	private static String resolvePathParameters(final String basePath, final String classPathAnnotation, final String methodPathAnnotation, final Map<String, Object> pathParameterValues)
	{
		final UriBuilder builder = UriBuilder.fromUri(basePath);
		if (classPathAnnotation != null && !classPathAnnotation.equals("")) {
			builder.path(classPathAnnotation);
		}
		if (methodPathAnnotation != null && !methodPathAnnotation.equals("")) {
			builder.path(methodPathAnnotation);
		}

		final URI resolvedUri = builder.buildFromMap(pathParameterValues);

		if (resolvedUri != null) {
			return resolvedUri.toString();
		}

		log.warn("Could not build a path URI for basePath={} and path annotations (class='{}',method='{}')", basePath,
				classPathAnnotation, methodPathAnnotation
		);
		return null;
	}
}
