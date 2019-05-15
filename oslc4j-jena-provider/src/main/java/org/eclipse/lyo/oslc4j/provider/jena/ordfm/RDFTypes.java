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

/**
 * 
 * @author rherrera
 */
public class RDFTypes {

    /**
     * The logger of this class.
     */
	private static final Logger LOGGER = LoggerFactory.getLogger(RDFTypes.class);

    /**
     * The set of scanned packages.
     */
    static final Set<String> SCANNED_PACKAGES = new HashSet<>();

    /**
     * The RDFs-Classes types mapping.
     */
    static final Map<String, List<Class<?>>> TYPES_MAPPINGS = new HashMap<>();

    /**
     * Scans a package (recursively) searching for classes annotated with
     * {@link OslcResourceShape} and mapping them with an {@link RDF#type} built
     * by the {@link TypeFactory#getQualifiedName(java.lang.Class)} method.
     * @param pkg the package to scan.
     */
    public synchronized static void mapPackage(Package pkg) {
        String packageName = pkg.getName();
        if (SCANNED_PACKAGES.contains(packageName)) {
            LOGGER.debug("> package {} already scanned", packageName);
        } else {
            int counter = 0;
            LOGGER.debug("> scanning package {}", packageName);
            ClassGraph classGraph = new ClassGraph().whitelistPackages(pkg.getName());
            classGraph = classGraph.enableClassInfo().enableAnnotationInfo();
            try (ScanResult scanResult = classGraph.scan()) {
                ClassInfoList classInforList = scanResult.getClassesWithAnnotation(OslcResourceShape.class.getName());
                for (ClassInfo classInfo : classInforList) {
                    if (classInfo.isAbstract()) {
                        LOGGER.warn("[-] Abstract class: " + classInfo.getName());
                    } else {
                        try {
                            Class<?> rdfClass = Class.forName(classInfo.getName());
                            String rdfType = TypeFactory.getQualifiedName(rdfClass);
                            List<Class<?>> types = TYPES_MAPPINGS.get(rdfType);
                            if (types == null) {
                                types = new ArrayList<>();
                                TYPES_MAPPINGS.put(rdfType, types);
                            }
                            types.add(rdfClass);
                            counter ++;
                            LOGGER.trace("[+] {} -> {}", rdfType, rdfClass);
                        } catch (ClassNotFoundException ex) {
                            LOGGER.warn("[-] Unexpected missing class: " + classInfo.getName());
                        }
                    }
                }
            }
            LOGGER.debug("< {} RDF classes found in package {}", counter, packageName);
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
        int size, index = 0;
        do {
            Class<?> pivot = candidates.get(index);
            Iterator<Class<?>> iterator = candidates.iterator();
            while(iterator.hasNext()) {
                Class<?> current = iterator.next();
                if (!current.equals(pivot) && current.isAssignableFrom(pivot)) {
                    iterator.remove();
                }
            }
            size = candidates.size();
        } while(++index < size);
        if (candidates.size() > 1) {
            Iterator<Class<?>> iterator = candidates.iterator();
            StringBuilder sb = new StringBuilder("Multiple classes, ");
            sb.append("not in the same inheritance tree, are annotated ");
            sb.append("to map the same RDF:type: ");
            sb.append(iterator.next().getName());
            while(iterator.hasNext()) {
                sb.append(", ");
                sb.append(iterator.next().getName());
            }
            throw new IllegalStateException(sb.toString());
        }
        return candidates.get(0);
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
        LOGGER.debug("> resolving class for resource {}", resource.getURI());
        StmtIterator rdfTypes = resource.listProperties(RDF.type);
        List<Class<?>> candidates = new ArrayList<>();
        synchronized(RDFTypes.class) {
            while(rdfTypes.hasNext()) {
                Statement statement = rdfTypes.nextStatement();
                String typeURI = statement.getObject().asResource().getURI();
                List<Class<?>> rdfClasses = TYPES_MAPPINGS.get(typeURI);
                if (rdfClasses == null) {
                    LOGGER.trace("[-] Unmapped class(es) for RDF:type {}", typeURI);
                } else if (rdfClasses.size() == 1) {
                    candidates.add(rdfClasses.get(0));
                    LOGGER.trace("[+] Candidate class {} found for RDF:type {}", rdfClasses.get(0).getName(), typeURI);
                } else if (preferredTypes.length == 0) {
                    StringBuilder sb = new StringBuilder();
                    sb.append("'preferredTypes' argument is required when more than one class (");
                    sb.append(rdfClasses.toString());
                    sb.append(") are mapped to the same RDF:type (");
                    sb.append(typeURI);
                    sb.append(")");
                    LOGGER.debug(sb.toString());
                    throw new IllegalArgumentException(sb.toString());
                } else {
                    for(Class<?> preferredType : preferredTypes) {
                        if (rdfClasses.contains(preferredType)) {
                            candidates.add(preferredType);
                            LOGGER.trace("[+] Preferred candidate class {} found for RDF:type {}", preferredType.getName(), typeURI);
                            break;
                        }
                    }
                }
            }
        }
        if (candidates.isEmpty()) {
            LOGGER.debug("< Unmapped class for resource {}", resource.getURI());
            return Optional.empty();
        } else {
            Class<?> mappedClass = (candidates.size() == 1 ? candidates.get(0) : getMostConcreteClassOf(candidates));
            LOGGER.debug("< Mapped class {} for resource {}", mappedClass.getName(), resource.getURI());
            return Optional.of(mappedClass);
        }
    }

}
