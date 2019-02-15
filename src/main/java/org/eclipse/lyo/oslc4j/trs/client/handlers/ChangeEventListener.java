package org.eclipse.lyo.oslc4j.trs.client.handlers;

import org.apache.jena.rdf.model.Model;
import org.eclipse.lyo.core.trs.ChangeEvent;

/**
 * Created on 2018-02-27
 *
 * @author Andrew Berezovskyi (andriib@kth.se)
 * @version $version-stub$
 * @since 0.0.1
 */
public interface ChangeEventListener {
    void handleChangeEvent(final ChangeEvent changeEvent, final Model trsResourceModel);
}
