package org.eclipse.lyo.server.provider.helper;

import java.lang.annotation.Annotation;
import java.net.InetAddress;
import java.net.URI;
import java.net.UnknownHostException;
import java.util.Collection;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.core.UriBuilder;

import org.eclipse.lyo.oslc4j.core.OSLC4JUtils;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNotQueryResult;
import org.eclipse.lyo.oslc4j.core.annotation.OslcQueryCapability;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.model.Compact;
import org.eclipse.lyo.oslc4j.core.model.IResource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

final public class ProviderHelper {
    private final static Logger log = LoggerFactory.getLogger(ProviderHelper.class);

    public final static int CANNOT_BE_DETERMINED_IN_ADVANCE = -1;

    static boolean hasNotQueryResultTypeAnnot(final Class<?> type) {
        Class<?> annotatedType = type;
        final Class<?> componentType = type.getComponentType();
        if(componentType != null) {
            annotatedType = componentType;
        }
        OslcNotQueryResult notQueryResult = annotatedType.getAnnotation(OslcNotQueryResult.class);
        return (notQueryResult != null && notQueryResult.value());
    }

    static boolean hasOslcQueryCapabilityMethodAnnot(final Annotation[] annotations) {
        if (annotations != null) {
            for (int i = 0; i < annotations.length; i++) {
                if (annotations[i] != null && annotations[i] instanceof OslcQueryCapability) {
                    return true;
                }
            }
        }
        return false;
    }

    public static boolean isQueryResult(final Class<?> type, final Annotation[] annotations) {
        return hasOslcQueryCapabilityMethodAnnot(annotations) && !hasNotQueryResultTypeAnnot(type);
    }

    public static boolean isSingleResourceType(final Class<?> type) {
        if (type == null) {
            throw new IllegalArgumentException();
        }
        if (type.isArray()) {
            return false;
        }
        if (Collection.class.isAssignableFrom(type)) {
            return false;
        }
        if (hasShapeAnnotation(type)) {
            return true;
        } else {
            if (IResource.class.isAssignableFrom(type)) {
                log.error("{} is missing @OslcResourceShape annotation", type.getName());
            }
            return false;
        }
    }

    private static boolean hasShapeAnnotation(final Class<?> type) {
        return type.getAnnotation(OslcResourceShape.class) != null;
    }

    public static boolean isCompactResource(final Class<?> type) {
        return Compact.class.isAssignableFrom(type);
    }

    public static boolean isOslcQuery(final String parmString)
    {
        boolean containsOslcParm = false;

        final String [] uriParts = parmString.toLowerCase().split("oslc\\.",2);
        if (uriParts.length > 1)
        {
            containsOslcParm = true;
        }

        return containsOslcParm;
    }
    
    /**
     * Resolve a URI (usually a resource subject or info URI) based on the settings of
     * org.eclipse.lyo.oslc4j.publicURI and org.eclipse.lyo.oslc4j.disableHostResolution.
     *
     * If the publicURI property is set, it takes precedence and is used to build the full URI.
     *
     * If the disableHostResolution property is false or not set, resolution of the local hostname
     * is attempted.
     *
     * If the disableHostResolution property is true or resolution has failed, the hostname is
     * retrieved from the request.
     *
     * Query parameters from the request are not copied to the resolved URI.
     *
     * @param request - request to base resolved URI on
     * @param includePath - if the path (after the context root) should be included in the
     * resolved URI
     * @return String containing the resolved URI
     * @deprecated Use {@link #resolveFullUri(HttpServletRequest)} instead.
     */
    @Deprecated
    public static String resolveURI(HttpServletRequest request, boolean includePath) {
        UriBuilder builder;

        final String pathInfo = request.getPathInfo();
        final String servletPath = request.getServletPath();
        final String configuredPublicURI = OSLC4JUtils.getPublicURI();

        if (configuredPublicURI != null && !configuredPublicURI.isEmpty()) {
            //public URI configured, use it - it includes the context
            String uriToBuild = configuredPublicURI;
            if (includePath) {
                uriToBuild = configuredPublicURI + "/" + servletPath + pathInfo;
            }
            builder = UriBuilder.fromUri(uriToBuild); //Normalize later
        } else {
            final String hostname = guessHostname(request);

            String contextPath = request.getContextPath();
            String pathToBuild = contextPath;
            if (includePath) {
                pathToBuild = contextPath + servletPath + pathInfo;
            }
            builder = UriBuilder.fromPath(pathToBuild)
                                .scheme(request.getScheme())
                                .host(hostname)
                                .port(request.getServerPort());
        }

        URI resolvedURI = builder.build().normalize();
        return resolvedURI.toString();
    }
    
    /**
     * Resolve a full request URI (usually a resource subject or info URI) based on the settings of
     * org.eclipse.lyo.oslc4j.publicURI and org.eclipse.lyo.oslc4j.disableHostResolution.
     *
     * If the publicURI property is set, it takes precedence and is used to build the full URI.
     *
     * If the servletPath property is set, it takes precedence and is used to build the full URI.
     *
     * If the disableHostResolution property is false or not set, resolution of the local hostname
     * is attempted.
     *
     * If the disableHostResolution property is true or resolution has failed, the hostname is
     * retrieved from the request.
     *
     * Query parameters from the request are not copied to the resolved URI.
     * @param request - request to base resolved URI on
     * @return String containing the resolved URI
     */
    public static String resolveFullUri(HttpServletRequest request) {
        final UriBuilder servletUriBuilder = servletUriBuilderFrom(request);

        final String pathInfo = request.getPathInfo();
        final UriBuilder publicUriBuilder = servletUriBuilder.path(pathInfo);

        URI resolvedURI = publicUriBuilder.build().normalize();
        return resolvedURI.toString();
    }

    /**
     * Resolve a servlet URI based on the settings of
     * org.eclipse.lyo.oslc4j.publicURI and org.eclipse.lyo.oslc4j.disableHostResolution.
     *
     * If the publicURI property is set, it takes precedence and is used to build the full URI.
     *
     * If the servletPath property is set, it takes precedence and is used to build the full URI.
     *
     * If the disableHostResolution property is false or not set, resolution of the local hostname
     * is attempted.
     *
     * If the disableHostResolution property is true or resolution has failed, the hostname is
     * retrieved from the request.
     *
     * Query parameters from the request are not copied to the resolved URI.
     * @param request - request to base resolved URI on
     * @return String containing the resolved URI
     */
    public static String resolveServletUri(HttpServletRequest request) {
        final UriBuilder servletUriBuilder = servletUriBuilderFrom(request);

        URI resolvedURI = servletUriBuilder.build().normalize();
        return resolvedURI.toString();
    }

    private static UriBuilder servletUriBuilderFrom(final String publicUri,
            final String servletPath) {
        return UriBuilder.fromUri(publicUri).path(servletPath);
    }
    
    private static UriBuilder servletUriBuilderFrom(final HttpServletRequest request) {
        final String publicUri = getOrConstructPublicUriBase(request);
        final String servletPath;
        if (OSLC4JUtils.getServletPath() != null) {
            servletPath = OSLC4JUtils.getServletPath();
        } else {
            servletPath = request.getServletPath();
        }

        return servletUriBuilderFrom(publicUri, servletPath);
    }

    private static String constructPublicUriBase(final String scheme, final String hostName,
            final int serverPort, final String contextPath) {
        return UriBuilder.fromPath(contextPath)
                         .scheme(scheme)
                         .host(hostName)
                         .port(serverPort)
                         .build()
                         .normalize()
                         .toString();
    }

    private static String getOrConstructPublicUriBase(final HttpServletRequest request) {
        String publicUri = OSLC4JUtils.getPublicURI();
        if (publicUri == null || publicUri.isEmpty()) {
            final String scheme = request.getScheme();
            final String hostName = guessHostname(request);
            final int serverPort = request.getServerPort();
            final String contextPath = request.getContextPath();
            publicUri = constructPublicUriBase(scheme, hostName, serverPort, contextPath);
        }
        return publicUri;
    }
    
    // TODO Andrew@2017-07-18: Avoid guessing anything and prefer configuration and/or convention
    @Deprecated
    private static String guessHostname(final HttpServletRequest request) {
        String hostName = "localhost";

        //try host resolution first if property to disable it is false or not set
        boolean getHostNameFromRequest = false;

        if (OSLC4JUtils.isHostResolutionDisabled()) {
            getHostNameFromRequest = true;
        } else {
            try {
                hostName = InetAddress.getLocalHost().getCanonicalHostName();
            } catch (UnknownHostException e) {
                //fallback is to use the hostname from request
                log.info("Unable to resolve hostname. Extracting hostname from request.");
                getHostNameFromRequest = true;
            }
        }

        if (getHostNameFromRequest) {
            hostName = request.getServerName();
        }
        return hostName;
    }
    
}
