package org.eclipse.lyo.core.trs;

import java.io.ByteArrayOutputStream;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RDFFormat;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Helper {
    private static final Logger log = LoggerFactory.getLogger(Helper.class);

    public static void printModelTrace(Model model) {
        String s = modelToString(model);
        log.trace(s); // .toString(StandardCharsets.UTF_8) for JDK10+
    }

    public static String modelToString(Model model) {
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        RDFDataMgr.write(baos, model, RDFFormat.TRIG_PRETTY);
        String s = baos.toString();
        return s;
    }
}
