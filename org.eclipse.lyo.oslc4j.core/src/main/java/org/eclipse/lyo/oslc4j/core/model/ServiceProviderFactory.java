/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
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
 *	   Michael Fiedler		- path parameter substitution
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.core.model;

import java.io.UnsupportedEncodingException;
import java.lang.reflect.Method;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLEncoder;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

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

public final class ServiceProviderFactory {
	private static final Logger logger = Logger.getLogger(ServiceProviderFactory.class.getName());

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

			handleResourceClass(baseURI, genericBaseURI, resourceClass, service, pathParameterValues);
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
						service.addCreationFactory(createCreationFactory(baseURI, method, pathParameterValues));
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

	private static CreationFactory createCreationFactory(final String baseURI, final Method method, final Map<String,Object> pathParameterValues) throws URISyntaxException {
		final OslcCreationFactory creationFactoryAnnotation = method.getAnnotation(OslcCreationFactory.class);

		final String title = creationFactoryAnnotation.title();
		final String label = creationFactoryAnnotation.label();
		final String[] resourceShapes = creationFactoryAnnotation.resourceShapes();
		final String[] resourceTypes = creationFactoryAnnotation.resourceTypes();
		final String[] usages = creationFactoryAnnotation.usages();

		final String basePath = baseURI + "/";

		final Path classPathAnnotation	= method.getDeclaringClass().getAnnotation(Path.class);
		String creation = resolvePathParameters(basePath, classPathAnnotation.value(), pathParameterValues);
		final Path methodPathAnnotation = method.getAnnotation(Path.class);
		if (methodPathAnnotation != null) {
			creation = creation + '/' + methodPathAnnotation.value();
		}

		CreationFactory creationFactory = null;
		creationFactory = new CreationFactory(title, new URI(creation).normalize());

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

	private static QueryCapability createQueryCapability(final String baseURI, final Method method, final Map<String,Object> pathParameterValues) throws URISyntaxException {
		final OslcQueryCapability queryCapabilityAnnotation = method.getAnnotation(OslcQueryCapability.class);

		final String title = queryCapabilityAnnotation.title();
		final String label = queryCapabilityAnnotation.label();
		final String resourceShape = queryCapabilityAnnotation.resourceShape();
		final String[] resourceTypes = queryCapabilityAnnotation.resourceTypes();
		final String[] usages = queryCapabilityAnnotation.usages();

		final String basePath = baseURI + "/";

		final Path classPathAnnotation	= method.getDeclaringClass().getAnnotation(Path.class);
		
		String creation = resolvePathParameters(basePath, classPathAnnotation.value(), pathParameterValues);
		
		final Path methodPathAnnotation = method.getAnnotation(Path.class);
		if (methodPathAnnotation != null) {
			creation = creation + '/' + methodPathAnnotation.value();
		}

		QueryCapability queryCapability = null;
		queryCapability = new QueryCapability(title, new URI(creation).normalize());

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

		final String classPathAnnotationValue = classPathAnnotation.value();

		if (dialogURI.length() > 0)
		{
			// TODO: Do we chop off everything after the port and then append the dialog URI?
			//		 For now just assume that the dialog URI builds on top of the baseURI.
			uri = resolvePathParameters(baseURI, dialogURI, pathParameterValues);
		}
		else
		{
			uri = genericBaseURI + "/generic/generic" + dialogType + ".html";


			final Path methodPathAnnotation = method.getAnnotation(Path.class);

			String parameter = resolvePathParameters(baseURI, classPathAnnotationValue, pathParameterValues);

			if (methodPathAnnotation != null)
			{
				parameter += '/' + methodPathAnnotation.value();
			}

			try
			{
				final String encodedParameter = URLEncoder.encode(parameter, "UTF-8");

				uri += "?" + parameterName + "=" + encodedParameter;
			}
			catch (final UnsupportedEncodingException exception)
			{
				logger.log(Level.WARNING,
						   "Error encoding URI [" +
						   parameter +
						   "]",
						   exception);
			}


			String resourceShapeParameters = "";

			if (resourceShapes != null)
			{
				final int numResourceShapes = resourceShapes.length;

				for (int index = 0; index < numResourceShapes; index++)
				{
					final String resourceShapeURI = baseURI + '/' + resourceShapes[index];

					try
					{
						final String encodedResourceShape = URLEncoder.encode(resourceShapeURI, "UTF-8");

						resourceShapeParameters += "&resourceShape=" + encodedResourceShape;
					}
					catch (final UnsupportedEncodingException exception)
					{
						logger.log(Level.WARNING,
								   "Error encoding URI [" +
								   resourceShapeURI +
								   "]",
								   exception);
					}
				}
			}

			uri += resourceShapeParameters;
		}

		Dialog dialog = null;
		dialog = new Dialog(title, new URI(uri).normalize());

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
	
	private static String resolvePathParameters(final String basePath, final String pathAnnotation, final Map<String, Object> pathParameterValues)
	{
		String returnUri = null;
		
		//Build the path from the @Path template + map of parameter value replacements
		if (pathParameterValues != null && pathParameterValues.size() > 0)
		{
			final UriBuilder builder = UriBuilder.fromUri(basePath);
			final URI resolvedUri = builder.path(pathAnnotation).buildFromMap(pathParameterValues);
			if (resolvedUri != null)
			{
				returnUri = resolvedUri.toString();
			}
			
		} 
		else
		{
			// no parameters supplied - assume @Path not templated
			returnUri = basePath + "/" + pathAnnotation;
		}
		return returnUri;
		
	}
}
