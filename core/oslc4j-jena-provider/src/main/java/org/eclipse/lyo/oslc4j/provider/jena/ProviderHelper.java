package org.eclipse.lyo.oslc4j.provider.jena;

import java.lang.annotation.Annotation;
import java.util.Collection;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNotQueryResult;
import org.eclipse.lyo.oslc4j.core.annotation.OslcQueryCapability;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.model.Compact;
import org.eclipse.lyo.oslc4j.core.model.IResource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

final class ProviderHelper {
    private final static Logger log = LoggerFactory.getLogger(ProviderHelper.class);

    final static int CANNOT_BE_DETERMINED_IN_ADVANCE = -1;

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
            for (Annotation annotation : annotations) {
                if (annotation != null && annotation instanceof OslcQueryCapability) {
                    return true;
                }
            }
        }
        return false;
    }

    static boolean isQueryResult(final Class<?> type, final Annotation[] annotations) {
        return hasOslcQueryCapabilityMethodAnnot(annotations) && !hasNotQueryResultTypeAnnot(type);
    }

    static boolean isSingleLyoResourceType(final Class<?> type) {
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

    static boolean isSingleResourceType(final Class<?> type) {
        if (type == null) {
            throw new IllegalArgumentException();
        }
        if (type.isArray()) {
            return false;
        }
        if (Collection.class.isAssignableFrom(type)) {
            return false;
        }
        return true;
    }

    private static boolean hasShapeAnnotation(final Class<?> type) {
        return type.getAnnotation(OslcResourceShape.class) != null;
    }

    static boolean isCompactResource(final Class<?> type) {
        return Compact.class.isAssignableFrom(type);
    }

    protected static boolean isOslcQuery(final String parmString)
    {
        boolean containsOslcParm = false;

        final String [] uriParts = parmString.toLowerCase().split("oslc\\.",2);
        if (uriParts.length > 1)
        {
            containsOslcParm = true;
        }

        return containsOslcParm;
    }

}
