/*-
 * Copyright (c) 2016-2017   KTH Royal Institute of Technology.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *
 * Omar Kacimi         -  Initial implementation
 * Andrew Berezovskyi  -  Lyo contribution updates
 */
package org.eclipse.lyo.oslc4j.trs.server;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.net.URISyntaxException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.namespace.QName;
import org.apache.commons.lang.math.RandomUtils;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.RDFWriter;
import org.apache.jena.rdf.model.ResIterator;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.vocabulary.RDF;
import org.eclipse.lyo.core.trs.Base;
import org.eclipse.lyo.core.trs.ChangeEvent;
import org.eclipse.lyo.core.trs.Creation;
import org.eclipse.lyo.core.trs.Deletion;
import org.eclipse.lyo.core.trs.Modification;
import org.eclipse.lyo.core.trs.Page;
import org.eclipse.lyo.core.trs.TRSConstants;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;

/**
 * A utility class for TRS opreations
 *
 * @version $version-stub$
 * @since 2.3.0
 */
public class TRSUtil {
    public static final SimpleDateFormat XSD_DATETIME_FORMAT;
    public static QName dateModifiedQname = new QName(OslcConstants.DCTERMS_NAMESPACE, "modified");

    static {
        XSD_DATETIME_FORMAT = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");
        XSD_DATETIME_FORMAT.setTimeZone(TimeZone.getDefault());
    }

    /**
     * Return the list of resources from the rdf model with the given rdf type
     *
     * @param model                    the rdf model in which the resources will be looked 4
     * @param fullQualifiedRDFTypeName the type of resources to look 4
     *
     * @return a list of rdf resources with the given type
     */
    public static List<Resource> getResourcesWithTypeFromModel(Model model,
            String fullQualifiedRDFTypeName) {
        List<Resource> resources = new ArrayList<Resource>();
        ResIterator listSubjects = null;
        listSubjects = model.listSubjectsWithProperty(
                RDF.type,
                model.getResource(fullQualifiedRDFTypeName));

        for (final ResIterator resIterator = listSubjects; resIterator.hasNext(); ) {
            Resource resource = resIterator.next();

            resources.add(resource);
        }

        return resources;
    }

    /**
     * look for and return the change events inside the given rdf model
     *
     * @param changeLogJenaModel the rdf model inside which the change events will be looked 4
     *
     * @return the list of change events found in the rdf model
     */
    public static List<ChangeEvent> getChangeEventsFromChangeLogJenaModel(Model changeLogJenaModel)
            throws IllegalAccessException, IllegalArgumentException, InstantiationException,
            InvocationTargetException, SecurityException, NoSuchMethodException,
            DatatypeConfigurationException, OslcCoreApplicationException, URISyntaxException {
        List<ChangeEvent> changeEvents = new ArrayList<ChangeEvent>();
        List<Resource> modificationResources = getResourcesWithTypeFromModel(changeLogJenaModel,
                                                                             TRSConstants
                                                                                     .TRS_TYPE_MODIFICATION);
        List<Resource> creationResources = getResourcesWithTypeFromModel(changeLogJenaModel,
                                                                         TRSConstants
                                                                                 .TRS_TYPE_CREATION);
        List<Resource> deletionResources = getResourcesWithTypeFromModel(changeLogJenaModel,
                                                                         TRSConstants
                                                                                 .TRS_TYPE_DELETION);

        for (Resource jenaRes : modificationResources) {
            ChangeEvent changeEvent = (Modification) JenaModelHelper.fromJenaResource(
                    jenaRes,
                    Modification.class);
            changeEvents.add(changeEvent);
        }

        for (Resource jenaRes : creationResources) {
            ChangeEvent changeEvent = (Creation) JenaModelHelper.fromJenaResource(
                    jenaRes,
                    Creation.class);
            changeEvents.add(changeEvent);
        }

        for (Resource jenaRes : deletionResources) {
            ChangeEvent changeEvent = (Deletion) JenaModelHelper.fromJenaResource(
                    jenaRes,
                    Deletion.class);
            changeEvents.add(changeEvent);
        }

        return changeEvents;
    }

    /**
     * return an rdf jena model object after reading it from the given file path
     *
     * @param filePath the path of the file from which the rdf model shall be read
     *
     * @return the rdf model read from the file
     */
    public static Model readJenaModelFromFile(String filePath) throws IOException {

        final Model rdFModel = ModelFactory.createDefaultModel();

        final InputStream is = new FileInputStream(filePath);
        rdFModel.read(is, null);
        is.close();

        return rdFModel;
    }

    /**
     * write the rdf model to the file with the given path in rdf/xml syntax
     *
     * @param jenaModel rdf model to be written to file
     * @param filePath  path to which the file shall be written 2
     */
    public static void writeModelToFile(Model jenaModel, String filePath) throws IOException {

        File file = new File(filePath);

        if (!file.exists()) {
            file.createNewFile();
        }

        final OutputStream os = new FileOutputStream(file);

        final RDFWriter rdfWriter = jenaModel.getWriter("RDF/XML");
        rdfWriter.setProperty("showXmlDeclaration", true);
        rdfWriter.setProperty("allowBadURIs", "true");
        rdfWriter.setProperty("relativeURIs", "");

        rdfWriter.write(jenaModel, os, null);
        os.flush();
        os.close();
    }

    /**
     * Convert a history data object to a change event and vice versa
     *
     * @param objToConvert the object to be converted
     *
     * @return the converted object
     */
    public static Object historyDataToChangeEvent(Object objToConvert) {

        if (objToConvert instanceof ChangeEvent) {
            ChangeEvent changeEvent = (ChangeEvent) objToConvert;
            URI changed = changeEvent.getChanged();
            Date modificationDate = (Date) changeEvent.getExtendedProperties()
                                                      .get(dateModifiedQname);
            HistoryData hd = null;
            if (changeEvent instanceof Deletion) {
                hd = HistoryData.getInstance(modificationDate, changed, HistoryData.DELETED);
            } else if (changeEvent instanceof Modification) {
                hd = HistoryData.getInstance(modificationDate, changed, HistoryData.MODIFIED);
            } else {
                hd = HistoryData.getInstance(modificationDate, changed, HistoryData.CREATED);
            }
            return hd;
        } else if (objToConvert instanceof HistoryData) {
            HistoryData hd = (HistoryData) objToConvert;

            ChangeEvent changeEvent = null;

            String hdType = hd.getType();
            Date timeStamp = hd.getTimestamp();
            URI changed = hd.getUri();
            int changeOrderInt = RandomUtils.nextInt();
            String changeOrder = String.valueOf(changeOrderInt);

            String changedUriString = "urn:urn-3:" + "cm1.example.com" + ":"
                    + XSD_DATETIME_FORMAT.format(hd.getTimestamp()) + ":" +
                    changeOrder;

            URI changedUri = URI.create(changedUriString);

            if (hdType.equals(HistoryData.CREATED)) {
                changeEvent = new Creation(changedUri, changed, changeOrderInt);
            } else if (hdType.equals(HistoryData.MODIFIED)) {
                changeEvent = new Modification(changedUri, changed, changeOrderInt);
            } else {
                changeEvent = new Deletion(changedUri, changed, changeOrderInt);
            }
            ;

            changeEvent.getExtendedProperties().put(dateModifiedQname, timeStamp);
            return changeEvent;
        }
        return null;
    }

    /**
     * The value for the link header returned for each response for a base page
     *
     * @return the link header value
     */
    // FIXME Andrew@2019-04-27: please please please
    public static String linkHeaderValue(Base base) {
        Page nextPage = base.getNextPage();
        URI pageOf = nextPage.getPageOf().getAbout();
        StringBuilder sb = new StringBuilder();
        final String newline = "\n";
        String headerValue = urize(pageOf.toString()) + "; rel=\"first\"," + newline + urize(
                nextPage.getNextPage().toString()) + "; rel=\"next\"," + newline + "<http://www"
                + "" + "" + "" + ".w3.org/ns/ldp#Page>; " + "rel=\"type\"" + newline;
        return headerValue;
    }

    public static String urize(String uri) {
        return "<" + uri + ">";
    }
}
