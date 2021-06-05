package org.eclipse.lyo.trs.client.util;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.util.Arrays;
import javax.ws.rs.core.Response;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.eclipse.lyo.core.trs.Base;
import org.eclipse.lyo.core.trs.ChangeLog;
import org.eclipse.lyo.core.trs.Creation;
import org.eclipse.lyo.core.trs.Deletion;
import org.eclipse.lyo.core.trs.Modification;
import org.eclipse.lyo.core.trs.Page;
import org.eclipse.lyo.core.trs.TrackedResourceSet;
import org.eclipse.lyo.oslc4j.core.exception.LyoModelException;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.eclipse.lyo.trs.client.exceptions.TrsEndpointConfigException;
import org.eclipse.lyo.trs.client.exceptions.TrsEndpointErrorException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * TODO
 *
 * @since TODO
 */
class ClientUtil {

    private final static Logger log = LoggerFactory.getLogger(ClientUtil.class);

    /**
     * extract the change log projo from the rdf model of the change log
     * returned by the server
     *
     * @param rdFModel of thr change log
     *
     * @return change log pojo
     */
    static ChangeLog extractChangeLogFromRdfModel(Model rdFModel) throws LyoModelException {
        log.debug("started extracting change log from rdf model");
        ChangeLog[] changeLogs;

        Modification[] modifications;
        Deletion[] deletions;
        Creation[] creations;

        changeLogs = JenaModelHelper.unmarshal(rdFModel, ChangeLog.class);
        creations = JenaModelHelper.unmarshal(rdFModel, Creation.class);
        modifications = JenaModelHelper.unmarshal(rdFModel, Modification.class);
        deletions = JenaModelHelper.unmarshal(rdFModel, Deletion.class);

        if (ProviderUtil.isNotEmptySingletonArray(changeLogs) && changeLogs[0] != null) {
            ChangeLog changeLog = changeLogs[0];
            changeLog.getChange().clear();
            if (ProviderUtil.isNotEmpty(modifications)) {
                changeLog.getChange().addAll(Arrays.asList((Modification[]) modifications));
            }

            if (ProviderUtil.isNotEmpty(creations)) {
                changeLog.getChange().addAll(Arrays.asList((Creation[]) creations));
            }

            if (ProviderUtil.isNotEmpty(deletions)) {
                changeLog.getChange().addAll(Arrays.asList((Deletion[]) deletions));
            }
            log.debug("finished extracting change log set from rdf model");
            return changeLog;
        } else {
            log.warn("the change log was missing; returning an empty one");
            return new ChangeLog();
        }
    }

    /**
     * extract the base projo from the rdf model of the base returned by the
     * server
     *
     * @param rdFModel of the base
     *
     * @return base pojo
     */
    static Base extractBaseFromRdfModel(Model rdFModel) throws LyoModelException {
        log.debug("started extracting base from rdf model");
        Page nextPage;
        Base baseObj = null;
        Object[] nextPageArray;
        Object[] basesArray;
        nextPageArray = JenaModelHelper.unmarshal(rdFModel, Page.class);
        basesArray = JenaModelHelper.unmarshal(rdFModel, Base.class);

        if (ProviderUtil.isNotEmptySingletonArray(basesArray) && basesArray[0] instanceof Base) {
            baseObj = (Base) basesArray[0];
        }

        if (baseObj == null) {
            log.error("Base page object is null");
            //FIXME nulls
//            throw new IllegalStateException();
            return null;
        }

        if (ProviderUtil.isNotEmptySingletonArray(nextPageArray) && nextPageArray[0] instanceof Page) {
            nextPage = (Page) nextPageArray[0];
            baseObj.setNextPage(nextPage);
        } else {
            log.debug("Base page {} is the last one", baseObj.getAbout());
        }
        log.debug("finished extracting base from rdf model");
        return baseObj;
    }

    /**
     * use osl4j functionality to retrieve a TRS object from the rdf model of
     * the TRS returned by the server
     *
     * @param rdFModel the rdf model
     *
     * @return the TRS pojo extracted from the TRS rdf model
     */
    static TrackedResourceSet extractTrsFromRdfModel(Model rdFModel)
            throws LyoModelException {
        log.debug("started extracting tracked resource set from rdf model");

        TrackedResourceSet[] trackedResourceSets = JenaModelHelper.unmarshal(rdFModel,
                TrackedResourceSet.class);

        if (ProviderUtil.isNotEmptySingletonArray(trackedResourceSets) && trackedResourceSets[0] != null) {
            TrackedResourceSet trs = trackedResourceSets[0];
            ChangeLog trsChangeLog = extractChangeLogFromRdfModel(rdFModel);
            try {
                trs.setChangeLog(trsChangeLog);
            } catch (URISyntaxException e) {
                // TODO https://github.com/eclipse/lyo.core/issues/102
                throw new IllegalStateException("Should never happen");
            }
            log.debug("finished extracting tracked resource set from rdf model");
            return trs;
        }
        throw new IllegalArgumentException("TRS resource cannot be extracted from the Model");
    }

    static Object extractResourceFromResponse(final Response response, final Class<?> objClass)
            throws TrsEndpointConfigException, TrsEndpointErrorException, LyoModelException {
        final Response.StatusType responseInfo = response.getStatusInfo();
        final Response.Status.Family httpCodeType = responseInfo.getFamily();
        if (httpCodeType.equals(Response.Status.Family.CLIENT_ERROR)) {
//            TODO these are not TRS exceptions but OSLC Client exceptions
            throw new TrsEndpointConfigException("Error " + responseInfo.getReasonPhrase());
        } else if (httpCodeType.equals(Response.Status.Family.SERVER_ERROR)) {
//            TODO these are not TRS exceptions but OSLC Client exceptions
            throw new TrsEndpointErrorException("Error " + responseInfo.getReasonPhrase());
        }
        if (AbstractResource.class.isAssignableFrom(objClass)) {
            Object objToRet = response.readEntity(objClass);
            log.trace("Finished consuming content from server response");
            return objToRet;
        } else if (Model.class.isAssignableFrom(objClass)) {
            return extractModelFromResponse(response);
        }

        throw new IllegalStateException("The resources could not be fetched");
    }

    /**
     * Extract and return a Jena model from the response if possible
     *
     * @param clientResponse response object from which the rdf model is read
     *
     */
    private static Model extractModelFromResponse(final Response clientResponse)
            throws LyoModelException {

        // FIXME Andrew@2019-07-15: proper exception handling
        if (clientResponse == null) {
            log.warn("The server response is null. Returning null");
            return null;
        }

        final String responseAsString = clientResponse.readEntity(String.class);
        log.trace("Response:\n{}\n", responseAsString);

        if (responseAsString == null) {
            log.warn("The server response is null. Returning null");
            return null;
        }

        log.trace("Creating Jena model from server response string");

        final Model rdFModel = ModelFactory.createDefaultModel();

        try {
            try (InputStream is = new ByteArrayInputStream(responseAsString.getBytes())) {
                rdFModel.read(is, null);
            }
        } catch (IOException e) {
            throw new LyoModelException(e);
        }

        log.trace("OK! Created Jena model from server response string");

        if (!rdFModel.isEmpty() && log.isDebugEnabled()) {
            log.debug("Created model contains {} statements", rdFModel.size());
        }

        return rdFModel;
    }

}
