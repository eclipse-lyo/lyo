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
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.core.model;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;

public final class InheritedMethodAnnotationHelper
{
	private InheritedMethodAnnotationHelper()
	{
		super();
	}
	
	public static <T extends Annotation> T getAnnotation(final Method	method,
														 final Class<T> annotationClass)
	{
		// First, try method for annotation

		final T annotation = method.getAnnotation(annotationClass);

		if (annotation != null)
		{
			return annotation;
		}

		final Class<?> declaringClass = method.getDeclaringClass();

		// Second, try superclass hierarchy for method annotation

		Class<?> currentSuperClass = declaringClass.getSuperclass();

		while (currentSuperClass != null)
		{
			try
			{
				final Method superClassMethod = currentSuperClass.getMethod(method.getName(),
																			method.getParameterTypes());

				final T superClassMethodAnnotation = superClassMethod.getAnnotation(annotationClass);

				if (superClassMethodAnnotation != null)
				{
					return superClassMethodAnnotation;
				}
			}
			catch (final Exception exception)
			{
				// Ignore and fall through to code below
			}

			currentSuperClass = currentSuperClass.getSuperclass();
		}

		// Third, try superclass' interface hierarchy for method annotation

		Class<?> currentClass = declaringClass;

		do
		{
			final Class<?>[] interfaces = currentClass.getInterfaces();

			for (final Class<?> interfac : interfaces)
			{
				final T interfaceMethodAnnotation = getRecursiveInterfaceMethodAnnotation(interfac,
																						  method,
																						  annotationClass);

				if (interfaceMethodAnnotation != null)
				{
					return interfaceMethodAnnotation;
				}
			}

			currentClass = currentClass.getSuperclass();
		}
		while (currentClass != null);

		return null;
	}

	private static <T extends Annotation> T getRecursiveInterfaceMethodAnnotation(final Class<?> interfac,
																				  final Method	 method,
																				  final Class<T> annotationClass)
	{
		try
		{
			final Method interfaceMethod = interfac.getMethod(method.getName(),
															  method.getParameterTypes());

			final T interfaceMethodAnnotation = interfaceMethod.getAnnotation(annotationClass);

			if (interfaceMethodAnnotation != null)
			{
				return interfaceMethodAnnotation;
			}
		}
		catch (final Exception exception)
		{
			// Ignore and fall through to code below
		}

		final Class<?>[] superInterfaces = interfac.getInterfaces();

		for (final Class<?> superInterface : superInterfaces)
		{
			final T interfaceMethodAnnotation = getRecursiveInterfaceMethodAnnotation(superInterface,
																					  method,
																					  annotationClass);

			if (interfaceMethodAnnotation != null)
			{
				return interfaceMethodAnnotation;
			}
		}

		return null;
	}
}