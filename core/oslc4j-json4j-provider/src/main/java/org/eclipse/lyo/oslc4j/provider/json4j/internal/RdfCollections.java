package org.eclipse.lyo.oslc4j.provider.json4j.internal;

import java.lang.reflect.InvocationTargetException;
import java.util.AbstractCollection;
import java.util.AbstractList;
import java.util.AbstractSequentialList;
import java.util.AbstractSet;
import java.util.Arrays;
import java.util.Collection;
import java.util.Deque;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.NavigableSet;
import java.util.Queue;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import org.eclipse.lyo.oslc4j.core.annotation.OslcRdfCollectionType;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;

/*
 * Various utilities for providers to handle RDF collections.
 */
public class RdfCollections {

    // Following constants are not defined in all versions of OslcRdfCollectionType
    public static final String RDF_LIST = "List";
    public static final String RDF_ALT = "Alt";
    public static final String RDF_BAG = "Bag";
    public static final String RDF_SEQ = "Seq";

    private static final Set<String> RDF_COLLECTION_TYPES = new HashSet<>(Arrays.asList(RDF_LIST, RDF_ALT, RDF_BAG, RDF_SEQ));

    /*
     * The type of List that represents a RDF sequence.
     * When extended properties are used, there is no OSLC annotation that helps at determining whether a given List is ordered (i.e. a sequence)
     * or unordered. We cannot assume a collection is ordered simply because it implements List.
     * Using a dedicated implementation class helps at being deterministic both for reading and writing content.
     *
     * Not setting a dedicated type for representing sequences implies:
     * - the writer will write all List instances as unordered collections
     * - the reader will read RDF sequences as ArrayList instances
     */
    private static Class<? extends List<?>> sequenceType;

    public static boolean isRdfCollection(OslcRdfCollectionType collectionType) {
        return OslcConstants.RDF_NAMESPACE.equals(collectionType.namespaceURI()) && RDF_COLLECTION_TYPES.contains(collectionType.collectionType());
    }

    /*
     * Sets the type of List used to represent RDF sequences in extended properties.
     * This type must define a public 0-arg constructor.
     */
    public static synchronized void setSequenceType(Class<? extends List<?>> sequenceType) {
        RdfCollections.sequenceType = sequenceType;
    }

    /*
     * Determines whether a specific implementation type was provided to support sequences.
     */
    public static boolean isSequenceSupported() {
        return sequenceType != null;
    }

    /*
     * Determines whether a specific implementation type was provided to support sequences
     * and the given collection matches that type.
     */
    public static synchronized boolean isSequence(Collection<?> collection) {
        return isSequenceSupported() && sequenceType.equals(collection.getClass());
    }

    /*
     * Creates a sequence List.
     * If a dedicated type was registered to represent sequences, an instance of such type is created.
     * Otherwise an UnsupportedOperationException is thrown. Developer is to check isSequenceSupported() first.
     */
    @SuppressWarnings("unchecked")
    public static <T> List<T> createSequence() {
        if (isSequenceSupported()) {
            try {
                return (List<T>) sequenceType.getDeclaredConstructor().newInstance();
            } catch (ReflectiveOperationException | IllegalArgumentException | SecurityException e) {
                throw new RuntimeException("Failed to create a sequence List", e); // NOSONAR
            }
        } else {
            // this is a developer error, the isSequenceSupported() method should have been called first
            throw new UnsupportedOperationException("No support for RDF:Seq was provided");
        }
    }

    /*
     * Creates a new Collection instance with an implementation type that matches the contract of the given interface type.
     */
    public static Collection<Object> createCollection(Class<?> type)
            throws InstantiationException, IllegalAccessException, InvocationTargetException, NoSuchMethodException {

        // Handle the Collection, List, Deque, Queue interfaces.
        // Handle the AbstractCollection, AbstractList, AbstractSequentialList classes
        if ((Collection.class == type) || (List.class == type) || (Deque.class == type) || (Queue.class == type) || (AbstractCollection.class == type)
                || (AbstractList.class == type) || (AbstractSequentialList.class == type)) {
            return new LinkedList<>();
        }

        // Handle the Set interface
        // Handle the AbstractSet class
        else if ((Set.class == type) || (AbstractSet.class == type)) {
            return new HashSet<>();
        }

        // Handle the SortedSet and NavigableSet interfaces
        else if ((SortedSet.class == type) || (NavigableSet.class == type)) {
            return new TreeSet<>();
        }

        // Not handled above. Let's try newInstance with possible failure.
        else {
            @SuppressWarnings("unchecked")
            Collection<Object> tempCollection = (Collection<Object>) type.getDeclaredConstructor().newInstance();
            return tempCollection;
        }
    }

    private RdfCollections() {
    }

}
