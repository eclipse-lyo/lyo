package org.eclipse.lyo.oslc4j.trs.server;

import java.net.URI;
import org.eclipse.lyo.oslc4j.core.model.IResource;

public interface ResourceEventHandler {
    void onCreated(IResource resource);

    void onModified(IResource resource);

    void onDeleted(URI resourceUri);
}
