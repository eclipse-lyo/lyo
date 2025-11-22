package org.eclipse.lyo.oslc4j.provider.jena;

import java.io.ByteArrayOutputStream;
import java.io.StringWriter;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.vocabulary.DCTerms;
import org.apache.jena.vocabulary.RDF;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class OslcXmlProviderTest {

    @Test
    public void testReifiedStatementDoesNotHideMainResource() {
        Model model = ModelFactory.createDefaultModel();

        String ns = "http://example.com/ns#";
        Resource mainResource = model.createResource(ns + "main");
        Property p = model.createProperty(ns + "p");
        Resource otherResource = model.createResource(ns + "other");

        // Main resource has a property
        mainResource.addProperty(DCTerms.title, "Main Resource");

        // A statement: mainResource p otherResource
        Statement s = model.createStatement(mainResource, p, otherResource);
        model.add(s);

        // Reify the statement
        Resource reifiedStmt = model.createResource(); // Anonymous reified statement
        reifiedStmt.addProperty(RDF.type, RDF.Statement);
        reifiedStmt.addProperty(RDF.subject, mainResource);
        reifiedStmt.addProperty(RDF.predicate, p);
        reifiedStmt.addProperty(RDF.object, otherResource);
        reifiedStmt.addProperty(DCTerms.title, "Reified Statement Title");

        // Write using RdfXmlAbbreviatedWriter
        RdfXmlAbbreviatedWriter writer = new RdfXmlAbbreviatedWriter();
        StringWriter sw = new StringWriter();
        writer.write(model, sw, null);

        String output = sw.toString();
        System.out.println(output);

        // Assert that main resource is present as a top-level description
        assertTrue(output.contains("rdf:about=\"http://example.com/ns#main\""), "Main resource should be present");
        assertTrue(output.contains(">Main Resource<"), "Main resource title should be present");
    }
}
