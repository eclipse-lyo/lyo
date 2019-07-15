/*
 * Copyright (c) 2017-2018 Xufei Ning and others.
 *
 * All rights reserved. This program and the accompanying materials are made available under the
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 which
 * accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html and the
 * Eclipse Distribution License is available at http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *
 * Xufei Ning          -  Initial implementation
 * Andrew Berezovskyi  -  Lyo contribution updates
 */

package org.eclipse.lyo.trs.client.mqtt;

import java.io.ByteArrayInputStream;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.ScheduledExecutorService;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.eclipse.lyo.core.trs.ChangeEvent;
import org.eclipse.lyo.core.trs.Creation;
import org.eclipse.lyo.core.trs.Deletion;
import org.eclipse.lyo.core.trs.Modification;
import org.eclipse.lyo.oslc4j.core.exception.LyoModelException;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.eclipse.lyo.trs.client.handlers.TrsProviderHandler;
import org.eclipse.lyo.trs.client.model.ChangeEventMessageTR;
import org.eclipse.paho.client.mqttv3.IMqttDeliveryToken;
import org.eclipse.paho.client.mqttv3.MqttCallback;
import org.eclipse.paho.client.mqttv3.MqttMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MqttTrsEventListener implements MqttCallback {

    private final static Logger log = LoggerFactory.getLogger(MqttTrsEventListener.class);
    private final TrsProviderHandler providerHandler;
    private final ScheduledExecutorService executorService;

    public MqttTrsEventListener(final TrsProviderHandler providerHandler, final ScheduledExecutorService executorService) {
        this.providerHandler = providerHandler;
        this.executorService = executorService;
    }

    public void connectionLost(Throwable throwable) {
        log.error("Connection with broker lost", throwable);
    }

    public void messageArrived(String s, MqttMessage mqttMessage) throws Exception {
        final String payload = new String(mqttMessage.getPayload());
        log.info("Message received: " + payload);
        if (payload.equalsIgnoreCase("NEW")) {
            log.warn("Plain 'NEW' ping message received");
            throw new IllegalArgumentException(
                    "'NEW' payload is no longer supported; send an RDF graph");
        }
        // receive ChangeEvent
        else {
            Runnable handleChangeEvent = () -> {
                log.info("Full ChangeEvent received");
                try {
                    if (payload.startsWith("<ModelCom")) {
                        throw new IllegalArgumentException(
                                "Malformed RDF from the serialised Jena Model; use RDFDataMgr");
                    } else {
                        final ChangeEventMessageTR eventMessage = unmarshalChangeEvent(payload);
                        providerHandler.processChangeEvent(eventMessage);
                    }
                } catch (LyoModelException e) {
                    log.warn("Error processing event", e);
                }
            };
            executorService.submit(handleChangeEvent);
        }
    }

    private ChangeEventMessageTR unmarshalChangeEvent(final String payload)
            throws LyoModelException {
        log.debug("MQTT payload: {}", payload);
        ChangeEvent changeEvent;
        final Model payloadModel = ModelFactory.createDefaultModel();
        final ByteArrayInputStream inputStream = new ByteArrayInputStream(payload.getBytes(
                StandardCharsets.UTF_8));
        RDFDataMgr.read(payloadModel, inputStream, Lang.JSONLD);

        // FIXME Andrew@2019-07-15: test the patch from Ricardo finally
        try {
            changeEvent = JenaModelHelper.unmarshalSingle(payloadModel, Modification.class);
        } catch (LyoModelException e) {
            try {
                changeEvent = JenaModelHelper.unmarshalSingle(payloadModel, Creation.class);
            } catch (LyoModelException e1) {
                try {
                    changeEvent = JenaModelHelper.unmarshalSingle(payloadModel, Deletion.class);
                } catch (LyoModelException e2) {
                    log.error("Can't unmarshal the payload", e2);
                    throw e2;
                }
            }
        }

        final Model trModel = ModelFactory.createDefaultModel();
        trModel.add(payloadModel);
        removeResource(changeEvent.getChanged(), trModel);

        return new ChangeEventMessageTR(changeEvent, trModel);
    }

    private void removeResource(final URI subject, final Model model) {
        model.removeAll(r(subject), null, null);
    }

    /**
     * Dummy Jena Resource for a URI. Can be to do raw ops on a model.
     */
    private Resource r(final URI tResourceUri) {
        return ResourceFactory.createResource(tResourceUri.toString());
    }

    public void deliveryComplete(IMqttDeliveryToken token) {
        log.trace("deliveryComplete: {}", token);
    }

}
