package org.eclipse.lyo.oslc4j.provider.jena;

import java.lang.annotation.Annotation;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNotQueryResult;
import org.eclipse.lyo.oslc4j.core.annotation.OslcQueryCapability;

/**
 * TODO
 *
 * @since TODO
 */
public class JenaProviderHelper {
    static boolean hasNotQueryResultTypeAnnot(final Class<?> type) {
        OslcNotQueryResult notQueryResult = type.getComponentType()
                .getAnnotation(OslcNotQueryResult.class);
        return (notQueryResult != null && notQueryResult.value());
    }

    static boolean hasOslcQueryCapabilityMethodAnnot(final Annotation[] annotations) {
        if (annotations != null) {
            for (int i = 0; i < annotations.length; i++) {
                if (annotations[i] != null && annotations[i] instanceof OslcQueryCapability) {
                    return true;
                }
            }
        }
        return false;
    }

    static boolean isQueryResult(final Class<?> type, final Annotation[] annotations) {
        return hasOslcQueryCapabilityMethodAnnot(annotations) &&
                !hasNotQueryResultTypeAnnot(type);
    }
}
