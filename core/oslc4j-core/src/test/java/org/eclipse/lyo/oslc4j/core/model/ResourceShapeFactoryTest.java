package org.eclipse.lyo.oslc4j.core.model;

import static org.assertj.core.api.Assertions.assertThat;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashSet;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreMissingSetMethodException;
import org.junit.Test;

/**
 * @version $version-stub$
 * @since 2.4.0
 */
public class ResourceShapeFactoryTest {

    @Test
    public void detectCollectionType() throws NoSuchMethodException {
        final boolean isCollectionType =
                ResourceShapeFactory.isCollectionType(
                        Dummy.class.getMethod("getValue").getReturnType());

        assertThat(isCollectionType).isTrue();
    }

    @Test
    public void detectNonCollectionType() throws NoSuchMethodException {
        final boolean isCollectionType =
                ResourceShapeFactory.isCollectionType(
                        Dummy.class.getMethod("getValue2").getReturnType());

        assertThat(isCollectionType).isFalse();
    }

    @Test
    public void findSetterNonCollection()
            throws NoSuchMethodException, OslcCoreMissingSetMethodException {
        ResourceShapeFactory.validateSetMethodExists(
                Dummy.class, Dummy.class.getMethod("getValue2"));
    }

    @Test
    public void findSetterCollection()
            throws NoSuchMethodException, OslcCoreMissingSetMethodException {
        ResourceShapeFactory.validateSetMethodExists(
                Dummy.class, Dummy.class.getMethod("getValue"));
    }

    @Test(expected = OslcCoreMissingSetMethodException.class)
    public void findSetterCollectionMismachGenType()
            throws NoSuchMethodException, OslcCoreMissingSetMethodException {
        ResourceShapeFactory.validateSetMethodExists(
                Dummy.class, Dummy.class.getMethod("getValueDifferent"));
    }

    interface Dummy {
        public ArrayList<BigDecimal> getValue();

        public void setValue(HashSet<BigDecimal> ds);

        public ArrayList<BigDecimal> getValueDifferent();

        public void setValueDifferent(HashSet<Integer> ds);

        public BigDecimal getValue2();

        public void setValue2(BigDecimal d);
    }
}
