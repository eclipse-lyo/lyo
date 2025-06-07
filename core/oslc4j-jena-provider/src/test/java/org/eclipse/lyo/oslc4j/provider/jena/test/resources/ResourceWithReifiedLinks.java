package org.eclipse.lyo.oslc4j.provider.jena.test.resources;

import java.net.URI;
import java.net.URISyntaxException;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.core.model.ValueType;

@OslcName("Test")
@OslcNamespace(TestResource.TEST_NAMESPACE)
@OslcResourceShape(title = "Test Resource", describes = TestResource.TEST_RESOURCE_TYPE)
public class ResourceWithReifiedLinks extends AbstractResource {
    public static final String TEST_NAMESPACE = "http://example.com/ns#";
    public static final String TEST_RESOURCE_TYPE = TEST_NAMESPACE + "Test";

    private String title;
    private Link linkWithNoLabel = new Link();
    private Link linkWithLabel = new Link();

    public ResourceWithReifiedLinks() throws URISyntaxException {
        super();
    }

    public ResourceWithReifiedLinks(final URI about) throws URISyntaxException {
        super(about);
    }

    @OslcName("linkWithNoLabel")
    @OslcPropertyDefinition(TEST_NAMESPACE + "linkWithNoLabel")
    @OslcValueType(ValueType.Resource)
    public Link getLinkWithNoLabel() {
        return linkWithNoLabel;
    }

    @OslcName("linkWithLabel")
    @OslcPropertyDefinition(TEST_NAMESPACE + "linkWithLabel")
    @OslcValueType(ValueType.Resource)
    public Link getLinkWithLabel() {
        return linkWithLabel;
    }

    @OslcName("title")
    @OslcPropertyDefinition(TEST_NAMESPACE + "title")
    @OslcValueType(ValueType.String)
    public String getTitle() {
        return title;
    }

    public void setTitle(final String title) {
        this.title = title;
    }

    public void setLinkWithNoLabel(final Link linkWithNoLabel) {
        this.linkWithNoLabel = linkWithNoLabel;
    }

    public void setLinkWithLabel(final Link linkWithLabel) {
        this.linkWithLabel = linkWithLabel;
    }
}
