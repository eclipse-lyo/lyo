package org.eclipse.lyo.core.trs;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RDFFormat;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.ByteArrayOutputStream;

public class Helper {
    private static final Logger log = LoggerFactory.getLogger(Helper.class);

    static void printModelTrace(Model model) {
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        RDFDataMgr.write(baos, model, RDFFormat.TRIG_PRETTY);
        log.trace(baos.toString()); // .toString(StandardCharsets.UTF_8) for JDK10+
    }
}
