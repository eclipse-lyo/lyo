/* SPDX-License-Identifier: EPL-2.0 OR BSD-3-Clause */
package org.eclipse.lyo.oslc4j.provider.jena;

import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.RecordComponent;
import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URI;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import org.apache.jena.rdf.model.RDFList;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.vocabulary.RDF;
import org.eclipse.lyo.oslc4j.core.annotation.OslcAbout;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.core.model.TypeFactory;
import org.eclipse.lyo.oslc4j.core.exception.LyoModelException;

/**
 * Canonical-constructor reader. Component predicates default to the record's
 * namespace plus their Java name; {@link OslcName} overrides the local name and
 * {@link OslcPropertyDefinition} overrides the complete predicate URI.
 * {@link OslcAbout} identifies the subject rather than an RDF predicate.
 * This prototype deliberately does not use mutable bean setters.
 */
final class RecordReader {
  private RecordReader() {}

  static Object read(Resource subject, Class<?> type)
      throws NoSuchMethodException, InvocationTargetException, InstantiationException, IllegalAccessException {
    RecordComponent[] components = type.getRecordComponents();
    Object[] arguments = new Object[components.length];
    Class<?>[] signature = new Class<?>[components.length];
    for (int i = 0; i < components.length; i++) {
      RecordComponent component = components[i];
      signature[i] = component.getType();
      if (component.getAccessor().isAnnotationPresent(OslcAbout.class)) {
        if (component.getType() != URI.class || !subject.isURIResource()) {
          throw new IllegalArgumentException("@OslcAbout requires a URI component and a URI subject");
        }
        arguments[i] = URI.create(subject.getURI());
        continue;
      }
      OslcPropertyDefinition definition = component.getAccessor().getAnnotation(OslcPropertyDefinition.class);
      // Explicit predicates allow components from another vocabulary. Otherwise
      // use the record's namespace and the component's name (or explicit alias).
      OslcName name = component.getAccessor().getAnnotation(OslcName.class);
      String predicate = definition != null ? definition.value()
          : TypeFactory.getNamespace(type) + (name != null ? name.value() : component.getName());
      List<RDFNode> nodes = subject.listProperties(subject.getModel().createProperty(predicate))
          .mapWith(statement -> statement.getObject()).toList();
      arguments[i] = value(nodes, component.getGenericType(), component.getName());
    }
    return type.getDeclaredConstructor(signature).newInstance(arguments);
  }

  private static Object value(List<RDFNode> nodes, Type type, String name) {
    if (type instanceof ParameterizedType generic
        && (generic.getRawType() == List.class || generic.getRawType() == Set.class)) {
      List<RDFNode> values = new ArrayList<>();
      for (RDFNode node : nodes) {
        if (node.isResource() && (node.equals(RDF.nil) || node.asResource().hasProperty(RDF.first))) {
          values.addAll(node.as(RDFList.class).asJavaList());
        } else {
          values.add(node);
        }
      }
      // Repeated RDF predicates are unordered. Sort only those; RDF list order is meaningful.
      if (nodes.stream().noneMatch(n -> n.isResource() && (n.equals(RDF.nil) || n.asResource().hasProperty(RDF.first)))) {
        values.sort(Comparator.comparing(RDFNode::toString));
      }
      List<Object> result = values.stream().map(n -> scalar(n, generic.getActualTypeArguments()[0])).toList();
      return generic.getRawType() == Set.class ? java.util.Collections.unmodifiableSet(new LinkedHashSet<>(result)) : result;
    }
    if (!(type instanceof Class<?> scalarType)) {
      throw new IllegalArgumentException("Unsupported record component type: " + type);
    }
    if (scalarType.isArray()) {
      Object array = Array.newInstance(scalarType.getComponentType(), nodes.size());
      for (int i = 0; i < nodes.size(); i++) Array.set(array, i, scalar(nodes.get(i), scalarType.getComponentType()));
      return array;
    }
    if (nodes.size() > 1) throw new IllegalArgumentException("Multiple values for scalar record component: " + name);
    if (nodes.isEmpty()) {
      if (scalarType.isPrimitive()) throw new IllegalArgumentException("Missing primitive record component: " + name);
      return null;
    }
    return scalar(nodes.get(0), scalarType);
  }

  private static Object scalar(RDFNode node, Type type) {
    if (type == Link.class) {
      if (!node.isURIResource()) throw new IllegalArgumentException("Expected link URI, received " + node);
      return new Link(URI.create(node.asResource().getURI()));
    }
    if (type == URI.class) {
      if (!node.isURIResource()) throw new IllegalArgumentException("Expected URI, received " + node);
      return URI.create(node.asResource().getURI());
    }
    if (node.isResource() && type instanceof Class<?> resourceType) {
      try {
        return JenaModelHelper.unmarshal(node.asResource(), resourceType);
      } catch (LyoModelException e) {
        throw new IllegalArgumentException("Cannot read nested resource as " + resourceType.getName(), e);
      }
    }
    if (!node.isLiteral()) throw new IllegalArgumentException("Expected literal for " + type + ": " + node);
    String lexical = node.asLiteral().getLexicalForm();
    if (type == String.class) return lexical;
    if (type == Boolean.class || type == boolean.class) {
      return switch (lexical) {
        case "true", "1" -> true;
        case "false", "0" -> false;
        default -> throw new IllegalArgumentException("Invalid Boolean: " + lexical);
      };
    }
    if (type == Integer.class || type == int.class) return Integer.valueOf(lexical);
    if (type == Long.class || type == long.class) return Long.valueOf(lexical);
    if (type == Double.class || type == double.class) return node.asLiteral().getDouble();
    if (type == BigInteger.class) return new BigInteger(lexical);
    if (type == BigDecimal.class) return new BigDecimal(lexical);
    throw new IllegalArgumentException("Unsupported record literal type: " + type);
  }
}
