package org.eclipse.lyo.core.trs;

import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.util.List;

import static org.assertj.core.api.Assertions.*;
import static org.junit.Assert.*;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.RDFNode;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.xml.datatype.DatatypeConfigurationException;

/**
 * Created on 2017-06-08
 * @author Andrew Berezovskyi (andriib@kth.se)
 * @version $version-stub$
 * @since 0.0.1
 */
public class ChangeLogTest {
    private final static Logger log = LoggerFactory.getLogger(ChangeLogTest.class);
    @Test
    public void changeEventListNotNullByDefault() throws Exception {
        ChangeLog changeLog = new ChangeLog();

        final List<ChangeEvent> changeEventList = changeLog.getChange();

        assertThat(changeEventList).isNotNull();
    }

    @Test(expected = IllegalArgumentException.class)
    public void changeEventListCantBeSetToNull() throws Exception {
        ChangeLog changeLog = new ChangeLog();
        log.info("Setting changeLog Change Event list to null");
        changeLog.setChange(null);
    }


    @Test
    public void changeLogModelContainsEvents() throws InvocationTargetException, DatatypeConfigurationException,
        OslcCoreApplicationException, IllegalAccessException {
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
