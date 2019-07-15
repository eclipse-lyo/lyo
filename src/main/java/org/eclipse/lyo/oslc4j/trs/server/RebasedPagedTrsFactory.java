package org.eclipse.lyo.oslc4j.trs.server;


public class RebasedPagedTrsFactory implements PagedTrsFactory {
    private final InmemPagedTrs pagedTrs;

    public RebasedPagedTrsFactory(final InmemPagedTrs pagedTrs) {this.pagedTrs = pagedTrs;}

    @Override
    public PagedTrs getPagedTrs() {
        return pagedTrs;
    }

    @Override
    public ResourceEventHandler getResourceEventHandler() {
        return pagedTrs;
    }
}
