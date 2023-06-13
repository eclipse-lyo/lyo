package org.eclipse.lyo.oslc4j.core;

import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;

/**
 * Not for use outside Lyo, signatures may change outside major releases.
 *
 * @since 5.0.1
 */
public class CoreHelper {
    public static Class<?> getActualTypeArgument(Type type) {
        if (type instanceof Class) {
            return (Class<?>) type;
        } else if (type instanceof TypeVariable) {
            return (Class<?>) ((TypeVariable<?>)type).getBounds()[0];
        }
        throw new IllegalArgumentException("You must pass either a Class or a (generic) TypeVariable");
    }
}
