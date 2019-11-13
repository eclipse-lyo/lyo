package org.eclipse.lyo.oslc4j.client;

public class OslcClientFactory {
    
    static public OslcOAuthClientBuilder oslcOAuthClientBuilder() {
        return new OslcOAuthClientBuilder();
    }
    
    static public OslcClientBuilder oslcClientBuilder() {
        return new OslcClientBuilder();
    }


}
