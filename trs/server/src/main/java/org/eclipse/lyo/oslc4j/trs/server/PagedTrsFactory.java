package org.eclipse.lyo.oslc4j.trs.server;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;

import javax.ws.rs.core.UriBuilder;

import org.eclipse.lyo.oslc4j.core.OSLC4JUtils;
import org.eclipse.lyo.oslc4j.trs.server.service.TrackedResourceSetService;

public class PagedTrsFactory {
	public InmemPagedTrs getInmemPagedTrs(final int basePageLimit, final int changelogPageLimit, final URI uriBase,
            final String baseRelativePath, final String changeLogRelativePath, final Collection<URI> baseResourceUris) {
		InmemPagedTrs pagedTrs = new InmemPagedTrs(basePageLimit, changelogPageLimit, uriBase, baseRelativePath, changeLogRelativePath, baseResourceUris);
        return pagedTrs;
		
	}

	public InmemPagedTrs getInmemPagedTrs (final int basePageLimit, final int changelogPageLimit, final Collection<URI> baseResourceUris) {
		return getInmemPagedTrs(basePageLimit, changelogPageLimit,
            UriBuilder.fromUri(OSLC4JUtils.getServletURI()).path(TrackedResourceSetService.RESOURCE_PATH).build(), 
            TrackedResourceSetService.BASE_PATH, TrackedResourceSetService.CHANGELOG_PATH, baseResourceUris);
	}

	public InmemPagedTrs getInmemPagedTrs (final int basePageLimit, final int changelogPageLimit) {
		return getInmemPagedTrs(basePageLimit, changelogPageLimit,
            UriBuilder.fromUri(OSLC4JUtils.getServletURI()).path(TrackedResourceSetService.RESOURCE_PATH).build(), 
            TrackedResourceSetService.BASE_PATH, TrackedResourceSetService.CHANGELOG_PATH, new ArrayList<URI>());
	}

	public InmemPagedTrs getInmemPagedTrs () {
		return getInmemPagedTrs(50, 50,
            UriBuilder.fromUri(OSLC4JUtils.getServletURI()).path(TrackedResourceSetService.RESOURCE_PATH).build(), 
            TrackedResourceSetService.BASE_PATH, TrackedResourceSetService.CHANGELOG_PATH, new ArrayList<URI>());
	}

}
