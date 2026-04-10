package org.eclipse.lyo.oslc4j.provider.json4j.internal;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.TreeMap;
import java.util.function.BiConsumer;

import javax.xml.namespace.QName;

import org.eclipse.lyo.oslc4j.core.OslcGlobalNamespaceProvider;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespaceDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcSchema;
import org.eclipse.lyo.oslc4j.core.model.IOslcCustomNamespaceProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/*
 * Enables collecting and generating namespace prefix/uri mappings.
 */
public class NamespaceMappings {

    private static final Logger LOGGER = LoggerFactory.getLogger(NamespaceMappings.class);

    private static final String GENERATED_PREFIX_START = "j.";

    /*
     * Creates an instance with no pre-configured mapping
     */
    public static NamespaceMappings empty() {
        return new NamespaceMappings();
    }

    /*
     * Creates an instance with some pre-configured mappings
     * that notifies the given consumer when a prefix/namespace mapping is added
     */
    public static NamespaceMappings of(Map<String, String> prefixToNamespace, BiConsumer<String, String> updateListener) {
        NamespaceMappings mappings = new NamespaceMappings();
        prefixToNamespace.forEach(mappings::addMapping);
        mappings.updateListener = updateListener;
        return mappings;
    }

    /*
     * Creates an instance with mappings contributed by OslcGlobalNamespaceProvider
     */
    public static NamespaceMappings global() {
        NamespaceMappings mappings = new NamespaceMappings();
        OslcGlobalNamespaceProvider.getInstance().getPrefixDefinitionMap().forEach(mappings::addMapping);
        return mappings;
    }

    private final Map<String, String> prefixToNamespace;
    private final Map<String, String> namespaceToPrefix;
    private BiConsumer<String, String> updateListener;

    private NamespaceMappings() {
        this.prefixToNamespace = new TreeMap<>();
        this.namespaceToPrefix = new HashMap<>();
    }

    public boolean containsPrefix(String prefix) {
        return this.prefixToNamespace.containsKey(prefix);
    }

    public Map<String, String> getMappings() {
        return Collections.unmodifiableMap(this.prefixToNamespace);
    }

    private void addMapping(String prefix, String namespace) {
        namespaceToPrefix.put(namespace, prefix);
        addUnidirectionalMapping(prefix, namespace);
    }

    private void addUnidirectionalMapping(String prefix, String namespace) {
        prefixToNamespace.put(prefix, namespace);
        if (updateListener != null) {
            updateListener.accept(prefix, namespace);
        }
    }

    /*
     * Adds a mapping for the given prefix/namespace.
     * Generates a new prefix based on the given one if it conflicts with an existing mapping.
     */
    public void addKnownMapping(String prefix, String namespace) {
        generateQName(namespace, "unused-local-part", prefix);
    }

    /*
     * Add a mapping for the given prefix/namespace.
     * Generates a new prefix based on the given one if it conflicts with an existing mapping.
     */
    public void addKnownMapping(QName qName) {
        generateQName(qName.getNamespaceURI(), qName.getLocalPart(), qName.getPrefix());
    }

    /*
     * Adds mappings based on OslcSchema annotations of the given type and all its super types
     */
    public void addMappings(Class<? extends Object> type) {

        // lookup the OSLC schema of the package defining the class
        OslcSchema oslcSchemaAnnotation = type.getPackage().getAnnotation(OslcSchema.class);
        if (oslcSchemaAnnotation != null) {

            // register each mapping of the schema
            for (OslcNamespaceDefinition oslcNamespaceDefinitionAnnotation : oslcSchemaAnnotation.value()) {
                String prefix = oslcNamespaceDefinitionAnnotation.prefix();
                String namespace = oslcNamespaceDefinitionAnnotation.namespaceURI();
                addMapping(prefix, namespace);
            }

            // Adding custom prefixes obtained from an implementation, if there is an implementation.
            Class<? extends IOslcCustomNamespaceProvider> customNamespaceProvider = oslcSchemaAnnotation.customNamespaceProvider();
            if (!customNamespaceProvider.isInterface()) {
                addCustomMappings(customNamespaceProvider);
            }
        }

        // recurse on super class
        Class<?> superClass = type.getSuperclass();
        if (superClass != null) {
            addMappings(superClass);
        }

        // recurse on implemented interfaces
        Class<?>[] interfaces = type.getInterfaces();
        if (interfaces != null) {
            Arrays.stream(interfaces).forEach(this::addMappings);
        }
    }

    private void addCustomMappings(Class<? extends IOslcCustomNamespaceProvider> customNamespaceProvider) {
        try {
            IOslcCustomNamespaceProvider customNamespaceProviderImpl = customNamespaceProvider.getDeclaredConstructor().newInstance();
            Map<String, String> customNamespacePrefixes = customNamespaceProviderImpl.getCustomNamespacePrefixes();
            if (null != customNamespacePrefixes) {
                for (Map.Entry<String, String> namespaceEntry : customNamespacePrefixes.entrySet()) {
                    addMapping(namespaceEntry.getKey(), namespaceEntry.getValue());
                }
            }
        } catch (ReflectiveOperationException | IllegalArgumentException | SecurityException e) {
            throw new RuntimeException("Failed to instantiate the custom namespace provider implementation: " // NOSONAR
                    + customNamespaceProvider.getClass().getName(), e);
        }
    }

    /*
     * Returns a QName that includes the prefix associated to the given namespace or a newly generated one if none
     */
    public QName generateQName(String namespace, String localPart) {
        Objects.requireNonNull(namespace);
        Objects.requireNonNull(localPart);
        return generateQNameForNamespace(namespace, localPart);
    }

    /*
     * Returns a QName for the given parameters which includes a prefix, which is either the one yet associated to the namespace,
     * the given suggested prefix if not yet associated to another namespace or a newly generated one if none
     */
    public QName generateQName(String namespace, String localPart, String prefix) {
        Objects.requireNonNull(namespace);
        Objects.requireNonNull(localPart);
        // prefix might be null

        // use the suggested prefix, if any
        if ((prefix != null) && !prefix.isEmpty()) {
            return generateQNameForPrefix(namespace, localPart, prefix);
        }

        // only use the namespace information
        else {
            return generateQNameForNamespace(namespace, localPart);
        }
    }

    /*
     * Prefix is known to be non-empty here
     */
    private QName generateQNameForPrefix(String namespace, String localPart, String prefix) {

        // lookup associated namespace, if any
        String existingNamespace = prefixToNamespace.get(prefix);

        // prefix already bound to a namespace?
        if (existingNamespace != null) {

            // the expected namespace?
            if (namespace.equals(existingNamespace)) {
                return new QName(namespace, localPart, prefix);
            } else {
                // let the developer know he uses an inappropriate prefix
                LOGGER.warn("Ignoring prefix {} which is bound to {} instead of {}", prefix, existingNamespace, namespace);

                // and keep going with namespace information only
                return generateQNameForNamespace(namespace, localPart);
            }
        }

        else {
            // lookup existing prefix associated to this namespace
            String existingPrefix = namespaceToPrefix.get(namespace);

            // no prefix yet associated?
            if (existingPrefix == null) {

                // register this suggested prefix
                addMapping(prefix, namespace);
                return generateQNameForNamespace(namespace, localPart);
            }

            // associated to a different prefix
            else {
                // It's uncommon but not invalid to have two prefixes for the same namespace.
                // Register this prefix->namespace association but don't change the opposite association
                addUnidirectionalMapping(prefix, namespace);

                return new QName(namespace, localPart, prefix);
            }
        }
    }

    private QName generateQNameForNamespace(String namespace, String localPart) {

        // empty namespace?
        if ((namespace == null) || namespace.isEmpty()) {
            LOGGER.warn("Ignoring empty namespace associated to local part: {}", localPart);
            return new QName(namespace, localPart);
        }

        // use the existing prefix associated to the namespace, if any
        String existingPrefix = namespaceToPrefix.get(namespace);
        if (existingPrefix != null) {
            return new QName(namespace, localPart, existingPrefix);
        }

        else {
            // generate a new prefix for the namespace
            String prefix = generatePrefix();
            addMapping(prefix, namespace);
            return new QName(namespace, localPart, prefix);
        }
    }

    private String generatePrefix() {
        int i = 0;
        String candidatePrefix;
        do {
            candidatePrefix = GENERATED_PREFIX_START + i;
            ++i;
        } while (containsPrefix(candidatePrefix));
        return candidatePrefix;
    }
}
