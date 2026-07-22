package org.eclipse.lyo.oslc4j.provider.json4j.internal;

import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/*
 * Collects values that are to assign using a set method that takes either an array or a collection as parameter.
 *
 * This is needed for Jena, as values for arrays are not required to be contiguous.
 *
 * This is needed for Json, as a single value may be present in the Json object
 * whereas an array or collection is expected by the Java class used for data binding.
 */
public class CollectionSetterInvoker {

    private final Object bean;
    private final Map<PropertyAccessor, List<Object>> accessorValues;

    /*
     * Creates an instance for the bean we are trying to populate.
     */
    public CollectionSetterInvoker(Object bean) {
        this.bean = bean;
        this.accessorValues = new HashMap<>();
    }

    /*
     * Collects a value to use with the set method corresponding to the given property definition.
     */
    public void add(PropertyAccessor accessor, Object value) {
        List<Object> values = accessorValues.computeIfAbsent(accessor, v -> new ArrayList<>());
        values.add(value);
    }

    /*
     * Invokes all set methods for which values have been collected
     */
    public void invokeAll() throws IllegalAccessException, InvocationTargetException, InstantiationException, NoSuchMethodException {

        for (Map.Entry<PropertyAccessor, List<Object>> accessorValuesEntry : accessorValues.entrySet()) {
            PropertyAccessor accessor = accessorValuesEntry.getKey();
            List<Object> values = accessorValuesEntry.getValue();
            Method setMethod = accessor.getSetter();
            Class<?> parameterClass = setMethod.getParameterTypes()[0];

            // array?
            if (parameterClass.isArray()) {
                Class<?> setMethodComponentParameterClass = parameterClass.getComponentType();

                // To support primitive arrays, we have to use Array reflection to
                // set individual elements. We cannot use Collection.toArray.
                // Array.set will unwrap objects to their corresponding primitives.
                Object array = Array.newInstance(setMethodComponentParameterClass, values.size());

                int index = 0;
                for (Object value : values) {
                    Array.set(array, index++, value);
                }

                setMethod.invoke(bean, new Object[] { array }); // NOSONAR java:S3878
            }

            // Else - we are dealing with a collection or a subclass of collection
            else {
                Collection<Object> collection = RdfCollections.createCollection(parameterClass);
                collection.addAll(values);
                setMethod.invoke(bean, collection);
            }
        }

        this.accessorValues.clear();
    }
}
