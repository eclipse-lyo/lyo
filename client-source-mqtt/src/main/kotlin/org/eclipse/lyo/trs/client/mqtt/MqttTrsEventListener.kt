/*
 * Copyright (c) 2017 Xufei Ning and others
 *
 * All rights reserved. This program and the accompanying materials are made available under the
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 which
 * accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html and the
 * Eclipse Distribution License is available at http://www.eclipse.org/org/documents/edl-v10.php.
 */

package org.eclipse.lyo.trs.client.mqtt

import org.apache.jena.rdf.model.Model
import org.apache.jena.rdf.model.ModelFactory
import org.apache.jena.rdf.model.Resource
import org.apache.jena.rdf.model.ResourceFactory
import org.apache.jena.riot.Lang
import org.apache.jena.riot.RDFDataMgr
import org.eclipse.lyo.core.trs.ChangeEvent
import org.eclipse.lyo.core.trs.Creation
import org.eclipse.lyo.core.trs.Deletion
import org.eclipse.lyo.core.trs.Modification
import org.eclipse.lyo.oslc4j.core.exception.LyoModelException
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper
import org.eclipse.lyo.trs.client.handlers.IPushProviderHandler
import org.eclipse.lyo.trs.client.model.ChangeEventMessageTR
import org.eclipse.paho.client.mqttv3.IMqttMessageListener
import org.eclipse.paho.client.mqttv3.MqttMessage
import org.slf4j.LoggerFactory
import java.io.ByteArrayInputStream
import java.net.URI
import java.nio.charset.StandardCharsets
import java.util.concurrent.Executors

class MqttTrsEventListener(
        private val providerHandler: IPushProviderHandler,
        private val lang: Lang) : IMqttMessageListener {
    private val log = LoggerFactory.getLogger(MqttTrsEventListener::class.java)
    private val executorService = Executors.newSingleThreadScheduledExecutor()

    override fun messageArrived(topic: String, mqttMessage: MqttMessage) {
        val payload = String(mqttMessage.payload)
        log.trace("Message payload: $payload")
        rejectLegacyPayloads(payload)
        executorService.submit {
            log.info("Processing Change Event")
            try {
                val eventMessage = unmarshalChangeEvent(payload)
                providerHandler.handlePush(eventMessage, topic)
            } catch (e: LyoModelException) {
                log.warn("Error processing Change Event", e)
            } catch (e: Exception) {
                log.error("Unexpected exception", e)
            }
        }
    }

    private fun rejectLegacyPayloads(payload: String) {
        if (payload.equals("NEW", ignoreCase = true)) {
            log.warn("Plain 'NEW' ping message received")
            throw IllegalArgumentException("'NEW' payload is no longer supported; send an RDF graph")
        }
        if (payload.startsWith("<ModelCom")) {
            throw IllegalArgumentException("Malformed RDF from the serialised Jena Model; use RDFDataMgr")
        }
    }

    private fun unmarshalChangeEvent(payload: String): ChangeEventMessageTR {
        var changeEvent: ChangeEvent
        val payloadModel = ModelFactory.createDefaultModel()
        val inputStream = ByteArrayInputStream(payload.toByteArray(StandardCharsets.UTF_8))
        RDFDataMgr.read(payloadModel, inputStream, lang)

        // FIXME Andrew@2019-07-15: test the patch from Ricardo finally
        try {
            changeEvent = JenaModelHelper.unmarshalSingle(payloadModel, Modification::class.java)
            log.debug("Encountered a Modification event")
        } catch (e: LyoModelException) {
            try {
                changeEvent = JenaModelHelper.unmarshalSingle(payloadModel, Creation::class.java)
                log.debug("Encountered a Creation event")
            } catch (e1: LyoModelException) {
                try {
                    changeEvent = JenaModelHelper.unmarshalSingle(payloadModel,
                            Deletion::class.java)
                    log.debug("Encountered a Deletion event")
                } catch (e2: LyoModelException) {
                    log.error("Can't unmarshal the payload", e2)
                    throw e2
                }
            }
        }

        val trModel = ModelFactory.createDefaultModel()
        trModel.add(payloadModel)
        removeResource(changeEvent.changed, trModel)

        return ChangeEventMessageTR(changeEvent, trModel)
    }

    private fun removeResource(subject: URI, model: Model) {
        model.removeAll(r(subject), null, null)
    }

    companion object {
        private fun r(resourceUri: URI): Resource {
            return ResourceFactory.createResource(resourceUri.toString())
        }
    }
}
