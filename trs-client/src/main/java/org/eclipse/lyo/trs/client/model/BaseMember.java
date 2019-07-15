package org.eclipse.lyo.trs.client.model;

import java.net.URI;
import org.apache.jena.rdf.model.Model;

public class BaseMember {
    private final URI uri;
    private final Model model;

    public BaseMember(final URI uri, final Model model) {
        this.uri = uri;
        this.model = model;
    }

    public URI getUri() {
        return uri;
    }

    public Model getModel() {
        return model;
    }
}
