package org.eclipse.lyo.server.common;

import jakarta.servlet.ServletContext;
import org.eclipse.microprofile.config.Config;
import org.eclipse.microprofile.config.ConfigProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Optional;

/**
 * Utility class for retrieving configuration properties.
 * <p>
 * This class uses MicroProfile Config (Jakarta Config) as the primary source of configuration,
 * falling back to ServletContext init parameters if the property is not found in the Config sources.
 */
public class LyoAppConfiguration {

    private static final Logger logger = LoggerFactory.getLogger(LyoAppConfiguration.class);

    private LyoAppConfiguration() {
        // Utility class
    }

    /**
     * Retrieves a configuration property using MicroProfile Config and falls back to ServletContext init parameters.
     * <p>
     * The search order is determined by the MicroProfile Config implementation (typically System Properties > Environment Variables > microprofile-config.properties),
     * followed by ServletContext init parameters.
     *
     * @param context the ServletContext to check for fallback (can be null)
     * @param key     the configuration key
     * @return the property value, or null if not found
     */
    public static String getOslcConfigProperty(ServletContext context, String key) {
        // 1. Check MicroProfile Config
        try {
            Config config = ConfigProvider.getConfig();
            Optional<String> value = config.getOptionalValue(key, String.class);
            if (value.isPresent()) {
                logger.debug("Found property '{}' in MicroProfile Config", key);
                return value.get();
            }
        } catch (Exception e) {
            // ConfigProvider might fail if no implementation is available
            logger.debug("MicroProfile Config not available or failed for key '{}': {}", key, e.getMessage());
        }

        // 2. Check Servlet Context
        if (context != null) {
            String initParam = context.getInitParameter(key);
            if (initParam != null) {
                logger.debug("Found property '{}' in ServletContext", key);
                return initParam;
            }
        }

        logger.debug("Property '{}' not found in configuration or ServletContext", key);
        return null;
    }
}
