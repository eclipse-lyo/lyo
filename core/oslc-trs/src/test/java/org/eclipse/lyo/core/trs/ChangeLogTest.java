package org.eclipse.lyo.core.trs;

import java.lang.reflect.InvocationTargetException;
import java.math.BigInteger;
import java.net.URI;
import java.util.List;

import static org.assertj.core.api.Assertions.*;
import static org.junit.Assert.*;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.vocabulary.RDF;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.jena.JenaModelHelper;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.xml.datatype.DatatypeConfigurationException;

public class ChangeLogTest {
    private final static Logger log = LoggerFactory.getLogger(ChangeLogTest.class);

    @Test
    public void changeEventListNotNullByDefault() throws Exception {
        ChangeLog changeLog = new ChangeLog();

        final List<ChangeEvent> changeEventList = changeLog.getChange();

        assertThat(changeEventList).isNotNull();
    }

    @Test
    public void testIntOrder() {
        Model model = ModelFactory.createDefaultModel();

        Resource resource = model.createResource("http://example.com/trs/c001");
        resource.addProperty(RDF.type, model.createResource(TRSConstants.TRS_TYPE_MODIFICATION));
        Property orderProp = model.createProperty(TRSConstants.TRS_ORDER);
        int value = Integer.MAX_VALUE - 1;
        resource.addLiteral(orderProp, value);

        log.info(Helper.modelToString(model));

        Modification modification = JenaModelHelper.unmarshalSingle(model, Modification.class);
        assertEquals(BigInteger.valueOf(value), modification.getOrder());
    }

    @Test
    public void testLongOrder() {
        Model model = ModelFactory.createDefaultModel();

        Resource resource = model.createResource("http://example.com/trs/c001");
        resource.addProperty(RDF.type, model.createResource(TRSConstants.TRS_TYPE_MODIFICATION));
        Property orderProp = model.createProperty(TRSConstants.TRS_ORDER);
        long value = Long.MAX_VALUE - 1;
        resource.addLiteral(orderProp, value);

        log.info(Helper.modelToString(model));

        Modification modification = JenaModelHelper.unmarshalSingle(model, Modification.class);
        assertEquals(BigInteger.valueOf(value), modification.getOrder());
    }

    @Test
    public void testBigintOrder() {
        Model model = ModelFactory.createDefaultModel();

        Resource resource = model.createResource("http://example.com/trs/c001");
        resource.addProperty(RDF.type, model.createResource(TRSConstants.TRS_TYPE_MODIFICATION));
        Property orderProp = model.createProperty(TRSConstants.TRS_ORDER);
        BigInteger value = BigInteger.valueOf(Long.MAX_VALUE).add(BigInteger.valueOf(1));
        resource.addLiteral(orderProp, value);

        log.info(Helper.modelToString(model));

        Modification modification = JenaModelHelper.unmarshalSingle(model, Modification.class);
        assertEquals(value, modification.getOrder());
    }


    @Test(expected = IllegalArgumentException.class)
    public void changeEventListCantBeSetToNull() throws Exception {
        ChangeLog changeLog = new ChangeLog();
        log.info("Setting changeLog Change Event list to null");
        changeLog.setChange(null);
    }


    @Test
    public void changeLogModelContainsEvents()
        throws InvocationTargetException, DatatypeConfigurationException, OslcCoreApplicationException,
        IllegalAccessException {
        // see https://github.com/eclipse/lyo/issues/83

        final ChangeLog log1 = new ChangeLog();
        log1.setAbout(URI.create("urn:trs:log1"));
        final URI ch1Uri = URI.create("urn:trs:ch1");
        final Deletion ch1 = new Deletion(ch1Uri, URI.create("urn:about:nothing"), 1);
        log1.getChange().add(ch1);
        final Model model = JenaModelHelper.createJenaModel(new Object[]{log1});

        Helper.printModelTrace(model);

        // <urn:trs:ch1> ?p ?o .
        assertTrue(model.contains(model.createResource(ch1Uri.toString()), null, (RDFNode) null));
    }

}
