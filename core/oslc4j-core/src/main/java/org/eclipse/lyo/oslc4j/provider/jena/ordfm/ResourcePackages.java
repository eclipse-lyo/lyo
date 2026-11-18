/*
 * Copyright (c) 2022 Contributors to the Eclipse Foundation
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
package org.eclipse.lyo.oslc4j.provider.jena.ordfm;

import io.github.classgraph.ClassGraph;
import io.github.classgraph.ClassInfo;
import io.github.classgraph.ClassInfoList;
import io.github.classgraph.ScanResult;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.vocabulary.RDF;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.model.TypeFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ResourcePackages {

    /**
     * The logger of this class.
     */
	private static final Logger log = LoggerFactory.getLogger(ResourcePackages.class);

    /**
     * The set of scanned packages.
     */
    static final Set<String> SCANNED_PACKAGES = new HashSet<>();

    /**
     * The RDFs-Classes types mapping.
     */
    static final Map<String, Set<Class<?>>> TYPES_MAPPINGS = new HashMap<>();

    private ResourcePackages() {}

    /**
     * Scans a package (recursively) searching for classes annotated with
     * {@link OslcResourceShape} and mapping them with an {@link RDF#type} built
     * by the {@link TypeFactory#getQualifiedName(java.lang.Class)} method.
     * @param pkg the package to scan.
     */
    public static synchronized void mapPackage(Package pkg) {
        String packageName = pkg.getName();
        if (SCANNED_PACKAGES.contains(packageName)) {
            log.trace("> package {} already scanned", packageName);
        } else {
            int counter = 0;
            log.trace("> scanning package {}", packageName);
            ClassGraph classGraph = new ClassGraph().acceptPackages(pkg.getName());
            classGraph = classGraph.enableClassInfo().enableAnnotationInfo();
            try (ScanResult scanResult = classGraph.scan()) {
                ClassInfoList classInforList = scanResult.getClassesWithAnnotation(OslcResourceShape.class.getName());
                for (ClassInfo classInfo : classInforList) {
                    if (classInfo.isAbstract()) {
                        log.trace("[-] Abstract class: {}", classInfo.getName());
                    } else {
                        try {
                            Class<?> rdfClass = Class.forName(classInfo.getName(), true,
                                Thread.currentThread().getContextClassLoader());
                            String rdfType = TypeFactory.getQualifiedName(rdfClass);
                            var types = TYPES_MAPPINGS.computeIfAbsent(rdfType, k -> new HashSet<>());
                            if(types.add(rdfClass)) {
                                counter ++;
                                log.trace("[+] {} -> {}", rdfType, rdfClass);
                            } else {
                                log.trace("[.] {} already registered", rdfClass.getName());
                            }
                        } catch (ClassNotFoundException ex) {
                            log.trace("[-] Unexpected missing class: {}", classInfo.getName());
                        }
                    }
                }
            }
            log.debug("< {} RDF classes found in package {}", counter, packageName);
            SCANNED_PACKAGES.add(packageName);
        }
    }

    /**
     * Tries to find the most concrete class of a list of candidates classes.
     * @param candidates the candidates list of classes to evaluate.
     * @return the most concrete class.
     * @throws IllegalStateException if candidates contains at least two classes
     * belonging to different inheritance trees.
     */
    private static Class<?> getMostConcreteClassOf(List<Class<?>> candidates) {
        List<Class<?>> concrete = candidates.stream().distinct().filter(candidate ->
                candidates.stream().noneMatch(other -> !candidate.equals(other)
                        && (candidate.isAssignableFrom(other)
                            || implementsRdfType(other, TypeFactory.getQualifiedName(candidate)))))
                .toList();
        if (concrete.size() != 1) {
            throw new IllegalStateException("Cannot select one concrete class for RDF types: "
                    + candidates.stream().map(Class::getName).distinct().sorted()
                        .collect(java.util.stream.Collectors.joining(", ")));
        }
        return concrete.get(0);
    }
    /** Lyo Designer represents secondary RDF parents with annotated Java interfaces. */
    private static boolean implementsRdfType(Class<?> candidate, String rdfType) {
        if (candidate == null || rdfType == null) return false;
        for (Class<?> contract : candidate.getInterfaces()) {
            if ((contract.getAnnotation(OslcResourceShape.class) != null
                    && rdfType.equals(TypeFactory.getQualifiedName(contract)))
                    || implementsRdfType(contract, rdfType)) return true;
        }
        return implementsRdfType(candidate.getSuperclass(), rdfType);
    }

    /**
     * Gets the corresponding most concrete class (if any) of a
     * {@link Resource resource}'s {@link RDF#type}.
     * @param resource the resource to resolve its type.
     * @param preferredTypes sometimes, the same RDF type is mapped by more than
     * one class, in such cases this parameter indicates the preferred type to
     * return in priority order. Avoid to use abstract types here.
     * @return an empty optional if there is not a mapped class for the given
     * resource; it wrapped instance otherwise.
     * @throws IllegalStateException if more than one class (not in the same
     * inheritance tree) is annotated to be mapped by the same {@code RDF:type}.
     */
    public static Optional<Class<?>> getClassOf(Resource resource, Class<?>... preferredTypes) {
        log.trace("> resolving class for resource {}", resource.getURI());
        StmtIterator rdfTypes = resource.listProperties(RDF.type);
        List<Class<?>> candidates = new ArrayList<>();
        synchronized(ResourcePackages.class) {
            while(rdfTypes.hasNext()) {
                Statement statement = rdfTypes.nextStatement();
                String typeURI = statement.getObject().asResource().getURI();
                var rdfClasses = TYPES_MAPPINGS.get(typeURI);
                if (rdfClasses == null) {
                    log.trace("[-] Unmapped class(es) for RDF:type {}", typeURI);
                } else if (rdfClasses.size() == 1) {
                    var candidate = rdfClasses.stream().findFirst().get();
                    candidates.add(candidate);
                    log.trace("[+] Candidate class {} found for RDF:type {}", candidate.getName(), typeURI);
                } else if (preferredTypes.length == 0) {
                    StringBuilder sb = new StringBuilder();
                    sb.append("'preferredTypes' argument is required when more than one class (");
                    sb.append(rdfClasses.toString());
                    sb.append(") are mapped to the same RDF:type (");
                    sb.append(typeURI);
                    sb.append(")");
                    log.debug(sb.toString());
                    throw new IllegalArgumentException(sb.toString());
                } else {
                    for(Class<?> preferredType : preferredTypes) {
                        if (rdfClasses.contains(preferredType)) {
                            candidates.add(preferredType);
                            log.trace("[+] Preferred candidate class {} found for RDF:type {}",
                                preferredType.getName(), typeURI);
                            break;
                        }
                    }
                }
            }
        }
        if (candidates.isEmpty()) {
            log.debug("< Unmapped class for resource {}", resource.getURI());
            return Optional.empty();
        } else {
            Class<?> mappedClass = (candidates.size() == 1 ? candidates.get(0) : getMostConcreteClassOf(candidates));
            log.debug("< Mapped class {} for resource {}", mappedClass.getName(), resource.getURI());
            return Optional.of(mappedClass);
        }
    }

    /**
     * Reset all package registration.
     *
     * @since 4.0.0.RC
     */
    public static void reset() {
        SCANNED_PACKAGES.clear();
        TYPES_MAPPINGS.clear();
    }
}
