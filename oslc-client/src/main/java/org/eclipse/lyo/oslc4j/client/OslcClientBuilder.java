package org.eclipse.lyo.oslc4j.client;

import javax.ws.rs.client.ClientBuilder;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class OslcClientBuilder {
    private String version;
    private ClientBuilder clientBuilder;

    private final static Logger log = LoggerFactory.getLogger(OslcClientBuilder.class);

    public OslcClientBuilder() {
        version = OSLCConstants.OSLC2_0;
        clientBuilder = null;
    }

    public OslcClientBuilder setVersion(String version) {
        this.version = version;
        return this;
    }

    public OslcClientBuilder setClientBuilder(ClientBuilder clientBuilder) {
        this.clientBuilder = clientBuilder;
        return this;
    }

    public IOslcClient build() {
        return new OslcClient(clientBuilder, version);
    }
}
