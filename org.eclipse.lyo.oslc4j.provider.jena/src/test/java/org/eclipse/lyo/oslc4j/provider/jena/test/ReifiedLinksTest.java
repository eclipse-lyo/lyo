package org.eclipse.lyo.oslc4j.provider.jena.test;

import static org.junit.Assert.*;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Iterator;
import java.util.List;

import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.eclipse.lyo.oslc4j.provider.jena.test.resources.ResourceWithReifiedLinks;
import org.junit.Test;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ReifiedStatement;

public class ReifiedLinksTest {
	
	static String uriOfLinkWithNoLabel = "http://example.com/link.with.no.label";
	static String uriOfLinkWithLabel = "http://example.com/link.with.label";

	public static ResourceWithReifiedLinks getResource() throws URISyntaxException
    {
    	ResourceWithReifiedLinks aResource = new ResourceWithReifiedLinks(new URI("http://example.com/someResource"));
		aResource.title = "Some Title";
		aResource.linkWithNoLabel = new Link(new URI(uriOfLinkWithNoLabel));
		Link l = new Link(new URI(uriOfLinkWithLabel));
		l.setLabel("some label for a link to reifie");
		aResource.linkWithLabel = l;

        return aResource;
    }

	@Test
	public void test() throws Exception {

		ResourceWithReifiedLinks aResource = getResource();
		Model model = JenaModelHelper.createJenaModel(new Object[]{aResource});

		List<ReifiedStatement> reifiedStatements = model.listReifiedStatements().toList();

		assertTrue("exactly 1 Reified Statement is expected", reifiedStatements.size() == 1);

		Boolean linkWithLabelIsDetected = false;
		for (Iterator<ReifiedStatement> iterator = reifiedStatements.iterator(); iterator.hasNext();) {
			ReifiedStatement reifiedStatement = iterator.next();
			if (reifiedStatement.getStatement().getObject().asResource().getURI().equals(uriOfLinkWithLabel)) {
				assertTrue("The Link with a label is not reified as expected", reifiedStatement.listProperties().toList().size() == 5);
				linkWithLabelIsDetected = true;
			}
			assertTrue("the Link with no label is beig reified, but ought not to be reified", !reifiedStatement.getStatement().getObject().asResource().getURI().equals(uriOfLinkWithNoLabel));
		}
		assertTrue("Links with label is not reified", linkWithLabelIsDetected);
	}
}
