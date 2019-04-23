package org.eclipse.lyo.oslc4j.trs.client.util;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import javax.ws.rs.core.Response;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.trs.client.exceptions.TrsEndpointConfigException;
import org.eclipse.lyo.oslc4j.trs.client.exceptions.TrsEndpointErrorExpection;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * TODO
 *
 * @since TODO
 */
public class ClientUtil {

    private final static Logger log = LoggerFactory.getLogger(ClientUtil.class);

    public static Object extractResourceFromResponse(final Response response,
            final Class<?> objClass)
            throws TrsEndpointConfigException, TrsEndpointErrorExpection, IOException {
        final Response.StatusType responseInfo = response.getStatusInfo();
        final Response.Status.Family httpCodeType = responseInfo.getFamily();
        if (httpCodeType.equals(Response.Status.Family.CLIENT_ERROR)) {
            throw new TrsEndpointConfigException("Error " + responseInfo.getReasonPhrase());
        } else if (httpCodeType.equals(Response.Status.Family.SERVER_ERROR)) {
            throw new TrsEndpointErrorExpection("Error " + responseInfo.getReasonPhrase());
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
     * @throws IOException thrown while reading from the response
     */
    private static Model extractModelFromResponse(final Response clientResponse)
            throws IOException {

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

        try (InputStream is = new ByteArrayInputStream(responseAsString.getBytes())) {
            rdFModel.read(is, null);
        }

        log.trace("OK! Created Jena model from server response string");

        if (!rdFModel.isEmpty() && log.isDebugEnabled()) {
            log.debug("Created model contains {} statements", rdFModel.size());
        }

        return rdFModel;
    }

}
