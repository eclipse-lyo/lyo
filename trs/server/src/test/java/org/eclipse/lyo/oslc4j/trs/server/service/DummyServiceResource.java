package org.eclipse.lyo.oslc4j.trs.server.service;

import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;

@Path("/test")
public class DummyServiceResource {
    @GET
    @Produces(MediaType.TEXT_PLAIN)
    public String testCall() {
        return "Hello world";
    }
}
