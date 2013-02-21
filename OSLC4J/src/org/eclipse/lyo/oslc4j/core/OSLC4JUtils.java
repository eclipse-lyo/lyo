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
 *     Michael Fiedler       - initial API and implementation
 *     
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.core;

import java.net.InetAddress;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.net.UnknownHostException;
import java.util.logging.Logger;

import javax.ws.rs.core.UriBuilder;
import javax.servlet.http.HttpServletRequest;

import org.eclipse.lyo.oslc4j.core.OSLC4JConstants;


public class OSLC4JUtils {
	private static String publicURI = System.getProperty(OSLC4JConstants.OSLC4J_PUBLIC_URI);
	
	private static final Logger logger = Logger.getLogger(OSLC4JUtils.class.getName());
	/**
	 * Returns the value of org.eclipse.lyo.oslc4j.publicURI or null if not set.
	 * 
	 * 
	 * @return
	 */
	public static String getPublicURI()
	{
		return publicURI;
	}
	
	/**
	 * Sets the value of org.eclipse.lyo.oslc4j.publicURI
	 * @param publicURI
	 */
	@SuppressWarnings("unused")
	public static void setPublicURI(String newPublicURI) throws MalformedURLException
	{

		if (newPublicURI != null && !newPublicURI.isEmpty())
		{
			//test for valid URL - exception will be thrown if invalid
			URL newPublicURL = new URL(newPublicURI);			
		}
		publicURI = newPublicURI;
	}
	
	/**
	 * Returns the boolean value of org.eclipse.lyo.oslc4j.disableHostResolution
	 * Default is false if not set or invalid (hostname resolution will take place)
	 * @return
	 */
	public static boolean isHostResolutionDisabled()
	{
		boolean retVal = false;
		
		String hostResDisabledProp = System.getProperty(OSLC4JConstants.OSLC4J_DISABLE_HOST_RESOLUTION);
		if (hostResDisabledProp !=null)
		{
			retVal = Boolean.parseBoolean(hostResDisabledProp);
		}
		return retVal;
		
	}
	
	public static void setHostResolutionDisabled(boolean hostResDisabled)
	{
		System.setProperty(OSLC4JConstants.OSLC4J_DISABLE_HOST_RESOLUTION, Boolean.toString(hostResDisabled));
	}
	
	
	/**
	 * Resolve a URI (usually a resource subject or info URI) based on the settings of
	 * org.eclipse.lyo.oslc4j.publicURI and org.eclipse.lyo.oslc4j.disableHostResolution.
	 * 
	 * If the publicURI property is set, it takes precedence and is used to build the full URI.
	 * 
	 * If the disableHostResolution property is false or not set, resolution of the local hostname is attempted.
	 * 
	 * If the disableHostResolution property is true or resolution has failed, the hostname is retrieved from the request.
	 * 
	 * Query parameters from the request are not copied to the resolved URI.
	 * 
	 * @param request - request to base resolved URI on
	 * @param includePath - if the path (after the context root) should be included in the resolved URI
	 * @return
	 */
	public static String resolveURI(HttpServletRequest request, boolean includePath)
	{
		UriBuilder builder = null;

		final String pathInfo    = request.getPathInfo();
	    final String servletPath = request.getServletPath();
		final String configuredPublicURI = getPublicURI();
	   
		//public URI configured, use it - it includes the context
		if (configuredPublicURI != null && !configuredPublicURI.isEmpty())
		{
			String uriToBuild = includePath ? (configuredPublicURI + "/" + servletPath + pathInfo) : configuredPublicURI;
			builder = UriBuilder.fromUri(uriToBuild); //Normalize later
		}
		else
		{
			String hostName = "localhost";
		   
			//try host resolution first if property to disable it is false or not set
			boolean getHostNameFromRequest = false;
		   
			if (isHostResolutionDisabled())
			{
			   getHostNameFromRequest = true;
			}
			else
			{
				try 
				{
					hostName = InetAddress.getLocalHost().getCanonicalHostName();
				}
				catch (UnknownHostException e)
				{
					//fallback is to use the hostname from request
					logger.finer("Unable to resolve hostname.  Extracting hostname from request.");
					getHostNameFromRequest = true;
				}
			}
		   
			if (getHostNameFromRequest)
			{
				hostName = request.getServerName();
			}

			String contextPath   = request.getContextPath();
			String pathToBuild   = includePath ? (contextPath + servletPath + pathInfo) : contextPath; 
            builder = UriBuilder.fromPath(pathToBuild)
                                .scheme(request.getScheme())
                                .host(hostName)
                                .port(request.getServerPort());			   
		}
		   
	   
	   
		URI resolvedURI = builder.build().normalize();
       
		return resolvedURI.toString();

	}
	
	/**
	 * Returns the boolean value of org.eclipse.lyo.oslc4j.disableRelativeURIs
	 * Default is true if not set or invalid (relative URIs will not be allowed)
	 * @return
	 */
	public static boolean relativeURIsAreDisabled()
	{
		boolean retVal = true;
		
		String relURIsDisabledProp = System.getProperty(OSLC4JConstants.OSLC4J_DISABLE_RELATIVE_URIS);
		if (relURIsDisabledProp !=null)
		{
			retVal = Boolean.parseBoolean(relURIsDisabledProp);
		}
		return retVal;
	}
}
