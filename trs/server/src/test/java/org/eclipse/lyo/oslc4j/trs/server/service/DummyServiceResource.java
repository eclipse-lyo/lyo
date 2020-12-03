package org.eclipse.lyo.oslc4j.trs.server.service;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

@Path("/test")
public class DummyServiceResource {
    @GET
    @Produces(MediaType.TEXT_PLAIN)
    public String testCall() {
        return "Hello world";
    }
}
