package org.eclipse.lyo.core.utils.marshallers;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.servlet.ServletContext;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;

public class LyoConfigUtil {
    private static final Logger log = LoggerFactory.getLogger(LyoConfigUtil.class);


    /**
     * For a property 'scheme', this is the lookup priority:
     * <p>
     * <ol>
     *     <li>LYO_SCHEME env variable</li>
     *     <li>%pkg_name%.scheme JVM property, e.g. org.eclipse.lyo.oslc4j.core.servlet.scheme</li>
     *     <li>%pkg_name%.scheme Servlet Context parameter, e.g. org.eclipse.lyo.oslc4j.core.servlet.scheme</li>
     * </ol>
     * @param key property key name
     * @param defaultValue default String value
     * @param klass Class of the ServletListener
     * @return value, if found, from ENV, JVM, or Servlet Context (in this order)
     */
    public static String getOslcConfigProperty(String key, String defaultValue, final ServletContext servletContext, Class klass) {
        String value = getParamFromEnvironment(generateEnvKey(key))
            .orElseGet(() -> getParamFromJvm(generatePackageBaseKey(klass, key))
                .orElseGet(() -> getParamFromContext(servletContext, generatePackageBaseKey(klass, key))
                    .orElse(defaultValue)));
        return value;
    }

    /**
     * For a property 'scheme', this is the lookup priority:
     * <p>
     * <ol>
     *     <li>LYO_SCHEME env variable</li>
     *     <li>%pkg_name%.scheme JVM property, e.g. org.eclipse.lyo.oslc4j.core.servlet.scheme</li>
     * </ol>
     * @param key property key name
     * @param defaultValue default String value
     * @param klass Class of the ServletListener
     * @return value, if found, from ENV or JVM (in this order)
     */
    public static String getOslcConfigPropertyNoContext(String key, String defaultValue, Class klass) {
        String value = getParamFromEnvironment(generateEnvKey(key))
            .orElseGet(() -> getParamFromJvm(generatePackageBaseKey(klass, key))
                    .orElse(defaultValue));
        return value;
    }

    /**
     * property name 'scheme' would become "org.eclipse.lyo.oslc4j.core.servlet.scheme"
     */
    public static String generatePackageBaseKey(Class klass, String key) {
        return klass.getPackage().getName() + '.' + key;
    }

    /**
     * web.xml property ending in '.scheme' would become "LYO_SCHEME"
     */
    public static String generateEnvKey(String key) {
        return "LYO_" + key.toUpperCase(Locale.ROOT).replace('.', '_');
    }


    public static Optional<String> getParamFromEnvironment(String basePathEnvKey) {
        final Map<String, String> env = System.getenv();
        if (!env.containsKey(basePathEnvKey)) {
            log.debug("ENV variable '{}' not defined", basePathEnvKey);
            return Optional.empty();
        }
        log.info("Found {} env variable", basePathEnvKey);
        return Optional.of(env.get(basePathEnvKey));
    }

    public static Optional<String> getParamFromJvm(String basePathContextPropertyKey) {
        String value = System.getProperty(basePathContextPropertyKey);
        if (value == null || value.trim().isEmpty()) {
            log.debug("System (JVM) property '{}' not defined", basePathContextPropertyKey);
            return Optional.empty();
        }
        log.info("Found {} System (JVM) property", basePathContextPropertyKey);
        return Optional.of(value);
    }

    public static Optional<String> getParamFromContext(final ServletContext servletContext, String basePathContextPropertyKey) {
        String value = servletContext.getInitParameter(basePathContextPropertyKey);
        if (value == null || value.trim().isEmpty()) {
            log.debug("Servlet Context parameter '{}' not defined", basePathContextPropertyKey);
            return Optional.empty();
        }
        log.info("Found {} context parameter", basePathContextPropertyKey);
        return Optional.of(value);
    }

    public static String getHost()
    {
        try
        {
            return InetAddress.getLocalHost().getCanonicalHostName();
        }
        catch (final UnknownHostException exception)
        {
            return "localhost";
        }
    }
}
