package org.eclipse.lyo.trs.client.config;

import java.net.URI;

public final class TrsProviderConfiguration {
    private final URI trsUri;
    private final String basicAuthUsername;
    private final String basicAuthPassword;

    public final URI getTrsUri() {
        return this.trsUri;
    }

    public final String getBasicAuthUsername() {
        return this.basicAuthUsername;
    }

    public final String getBasicAuthPassword() {
        return this.basicAuthPassword;
    }

    public TrsProviderConfiguration(URI trsUri, String basicAuthUsername, String basicAuthPassword) {
        super();
        this.trsUri = trsUri;
        this.basicAuthUsername = basicAuthUsername;
        this.basicAuthPassword = basicAuthPassword;
    }

    public static final TrsProviderConfiguration forHttp(String trsEndpointUri) {
        URI trsUri = URI.create(trsEndpointUri);
        return new TrsProviderConfiguration(trsUri, null, null);
    }

    public static final TrsProviderConfiguration forHttpWithBasicAuth(String trsEndpointUri, String trsEndpointUsername, String trsEndpointPassword) {
        URI trsUri = URI.create(trsEndpointUri);
        return new TrsProviderConfiguration(trsUri, trsEndpointUsername, trsEndpointPassword);
    }

}
