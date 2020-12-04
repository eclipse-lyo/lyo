/*
 * Copyright (c) 2020 Contributors to the Eclipse Foundation
 *
 * See the NOTICE file(s) distributed with this work for additional
 * information regarding copyright ownership.
 *
 * This program and the accompanying materials are made available under the
 * terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0, or the Eclipse Distribution License 1.0
 * which is available at http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Simple
 */
package org.eclipse.lyo.oslc4j.trs.server;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicLong;
import javax.ws.rs.core.UriBuilder;
import org.eclipse.lyo.core.trs.Base;
import org.eclipse.lyo.core.trs.ChangeEvent;
import org.eclipse.lyo.core.trs.ChangeLog;
import org.eclipse.lyo.core.trs.Creation;
import org.eclipse.lyo.core.trs.Deletion;
import org.eclipse.lyo.core.trs.Modification;
import org.eclipse.lyo.core.trs.Page;
import org.eclipse.lyo.core.trs.TRSConstants;
import org.eclipse.lyo.oslc4j.core.model.IResource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class is the backbone of the Tracked resource set service class. This class is extended by
 * oslc adapters wishing to implement an OSLC TRS interface. The implementing classes need to
 * implement one method returning the change history of the resources living in the tool exposed by
 * the OSLC adapter
 *
 * @version $version-stub$
 * @since 2.3.0
 */
public class InmemPagedTrs implements PagedTrs, TrsEventHandler {
    private final static Logger log = LoggerFactory.getLogger(InmemPagedTrs.class);

    /**
     * Max items per changelog Page
     */
    private final int changelogPageLimit;

    /**
     * Base in 'uriBase' has nothing to do with the TRS Base.
     */
    private final URI uriBase;

    private final AtomicLong trsOrderId = new AtomicLong();

    /**
     * Max items per base Page
     */
    private final int basePageLimit;

    /**
     * The relative path of the base, may contain URI template parameters.
     */
    private final String baseRelativePath;

    /**
     * The relative path of the changeLog, may contain URI template parameters.
     */
    private final String changeLogRelativePath;

    /**
     * List of base resources
     */
    private final List<Base> baseResources = new ArrayList<>();

    /**
     * List of Change Logs
     */
    private final List<ChangeLog> changelogResources = new ArrayList<>();

    /**
     * @param basePageLimit      Max items per Base page
     * @param changelogPageLimit Max items per Changelog page
     * @param uriBase            Set it via eg <pre>UriBuilder.fromUri(OSLC4JUtils.getServletURI()).path("trs").build()</pre>
     * @param baseRelativePath   The relative path of the base, may contain URI template parameters.
     * @param changeLogRelativePath   The relative path of the changeLog, may contain URI template parameters.
     * @param baseResourceUris   Initial set of the TRS Base resource URIs
     */
    public InmemPagedTrs(final int basePageLimit, final int changelogPageLimit, final URI uriBase,
            final String baseRelativePath, final String changeLogRelativePath, final Collection<URI> baseResourceUris) {
        this.basePageLimit = basePageLimit;
        this.changelogPageLimit = changelogPageLimit;
        this.uriBase = uriBase;
        this.baseRelativePath = baseRelativePath;
        this.changeLogRelativePath = changeLogRelativePath;
        initBase(baseResourceUris);
    }

    /**
     * @param basePageLimit      Max items per Base page
     * @param changelogPageLimit Max items per Changelog page
     * @param uriBase            Set it via eg <pre>UriBuilder.fromUri(OSLC4JUtils.getServletURI()).path("trs").build()</pre>
     * @param baseResourceUris   Initial set of the TRS Base resource URIs
     */
    public InmemPagedTrs(final int basePageLimit, final int changelogPageLimit, final URI uriBase,
            final Collection<URI> baseResourceUris) {
        this.basePageLimit = basePageLimit;
        this.changelogPageLimit = changelogPageLimit;
        this.uriBase = uriBase;
        this.baseRelativePath = "base";
        this.changeLogRelativePath = "changelog";
        initBase(baseResourceUris);
    }

    @Override
    public Base getBaseResource(final Integer pageId) {
        int listIdx = pageIdToListIdx(pageId);
        if (listIdx  < 0 || listIdx >= baseResources.size()) {
            throw new IllegalArgumentException("There is no such Base page");
        }
        return baseResources.get(listIdx);
    }

    private int pageIdToListIdx(final Integer pageId) {
        return pageId - 1;
    }

    @Override
    public int basePageCount() {
        return baseResources.size();
    }

    @Override
    public ChangeLog getChangeLog(final Integer pageId) {
        int listIdx = pageIdToListIdx(pageId);
        if (listIdx < 0 || listIdx >= changelogResources.size()) {
            throw new IllegalArgumentException("There is no such ChangeLog page");
        }
        return changelogResources.get(listIdx);
    }

    @Override
    public ChangeLog getChangeLogLast() {
        return getChangeLog(changelogPageCount());
    }

    @Override
    public int changelogPageCount() {
        return changelogResources.size();
    }

    @Override
    public void onCreated(final IResource resource) {
        final HistoryData instance = HistoryData.getInstance(new Date(), resource.getAbout(),
                HistoryData.CREATED);
        onHistoryData(instance);
    }

    @Override
    public void onModified(final IResource resource) {
        final HistoryData instance = HistoryData.getInstance(new Date(), resource.getAbout(),
                HistoryData.MODIFIED);
        onHistoryData(instance);
    }

    @Override
    public void onDeleted(final URI resourceUri) {
        final HistoryData instance = HistoryData.getInstance(new Date(), resourceUri,
                HistoryData.DELETED);
        onHistoryData(instance);
    }

    public void onHistoryData(final HistoryData event) {
        final ChangeLog changeLog = findOrCreateChangelogPage();
        final ChangeEvent changeEvent = createChangeEvent(event);
        changeLog.getChange().add(changeEvent);
    }

    private ChangeEvent createChangeEvent(final long changeOrder, final URI trackedResourceUri,
            final URI eventUri, final String histDataType) {
        final ChangeEvent ce;
        if (changeOrder >= Integer.MAX_VALUE) {
            throw new IllegalStateException("Switch ChangeEvents to use longs");
        }
        if (Objects.equals(histDataType, HistoryData.CREATED)) {
            ce = new Creation(eventUri, trackedResourceUri, (int) changeOrder);
        } else if (Objects.equals(histDataType, HistoryData.MODIFIED)) {
            ce = new Modification(eventUri, trackedResourceUri, (int) changeOrder);
        } else if (Objects.equals(histDataType, HistoryData.DELETED)) {
            ce = new Deletion(eventUri, trackedResourceUri, (int) changeOrder);
        } else {
            log.error("Change Event {} has unknown kind: {}", trackedResourceUri, histDataType);
            throw new IllegalArgumentException();
        }
        return ce;
    }

    private ChangeEvent createChangeEvent(final HistoryData event) {
        final long order = nextCutoff();
        final URI ceUri = createUuidUrn();

        if (order >= Integer.MAX_VALUE) {
            throw new IllegalStateException("Switch ChangeEvents to use longs");
        }
        return createChangeEvent((int) order, event.getUri(), ceUri, event.getType());
    }

    private void initBase(final Collection<URI> baseResourceUris) {
        Base base = this.findOrCreateBase();
        int remainingResources = calcRemainingResources(base);
        for (final URI resourceUris : baseResourceUris) {
            if (remainingResources < 1) {
                base = this.findOrCreateBase();
                remainingResources = calcRemainingResources(base);
            }
            base.getMembers().add(resourceUris);
            remainingResources -= 1;
        }
    }

    private Base findOrCreateBase() {
        final Base page;
        if (this.baseResources.isEmpty()) {
            page = createBase();
        } else {
            final Base lastBase = getLastBaseResource();
            if (isBaseFull(lastBase)) {
                page = createBase();
                lastBase.getNextPage().setNextPage(page.getNextPage().getAbout());
            } else {
                page = lastBase;
            }
        }
        // using single-return style to benefit from the 'final' check
        return page;
    }

    /**
     * Create a new page object with the page basepagenum
     *
     * @return the created base page object
     */
    private Base createBase() {
        final Base base = new Base();
        base.setAbout(this.createBaseUri());
        base.setNextPage(createBasePage(base, nextBasePageId()));
        base.setCutoffEvent(URI.create(TRSConstants.RDF_NIL));
        log.debug("Adding a new Base resource");
        baseResources.add(base);
        return base;
    }

    /**
     * Create the page attribute of the base object
     *
     * @param base   the base object for which this is the page
     * @param pageId number of the page
     *
     * @return the page of the base
     */
    private Page createBasePage(final Base base, final int pageId) {
        final Page basePage = new Page();
        basePage.setAbout(this.createBasePageUri(pageId));
        basePage.setNextPage(URI.create(TRSConstants.RDF_NIL));
        basePage.setPageOf(base);
        return basePage;
    }

    //the last page of the changeLog's URI is set to null, since it needs to be a local resource in the trackedResourceSet.
    //All other pages will have a URI
    private ChangeLog findOrCreateChangelogPage() {
        final ChangeLog page;
        if (this.changelogResources.isEmpty()) {
            page = createChangelogPage(TRSUtil.NIL_URI, null);
        } else {
            final ChangeLog lastPage = getLastChangelogPage();
            if (isChangelogPageFull(lastPage)) {
                lastPage.setAbout(createChangelogUri());
                page = createChangelogPage(lastPage.getAbout(), null);
            } else {
                page = lastPage;
            }
        }
        // using single-return style to benefit from the 'final' check
        return page;
    }

    private ChangeLog createChangelogPage(final URI previous, final URI current) {
        final ChangeLog changelog = new ChangeLog();
        changelog.setAbout(current);
        changelog.setPrevious(previous);

        changelogResources.add(changelog);
        return changelog;
    }

    private int calcRemainingResources(final Base base) {
        final int remaining = basePageLimit - base.getMembers().size();
        if (remaining < 0) {
            log.warn("Base contains more resources than it should!");
        }
        return Math.max(remaining, 0);
    }

    private boolean isBaseFull(final Base base) {
        return calcRemainingResources(base) == 0;
    }

    private boolean isChangelogPageFull(final ChangeLog changeLog) {
        return changeLog.getChange().size() >= changelogPageLimit;
    }

    private ChangeLog getLastChangelogPage() {
        return this.changelogResources.get(this.changelogResources.size() - 1);
    }

    private int nextBasePageId() {
        return basePageIdFor(this.baseResources.size());
    }

    private int basePageIdFor(final int listIdx) {
        return listIdx + 1;
    }

    private Base getLastBaseResource() {
        return this.baseResources.get(this.baseResources.size() - 1);
    }

    private URI createBaseUri() {
        final URI uri = getUriBuilder().path(this.baseRelativePath).build();
        return uri;
    }

    private URI createBasePageUri(final int pageId) {
        final URI uri = getUriBuilder().path(this.baseRelativePath).path(String.valueOf(pageId)).build();
        return uri;
    }

    private URI createChangelogUri() {
        final int nextPageId = this.changelogResources.size();
        final URI uri = getUriBuilder().path(this.changeLogRelativePath).path(String.valueOf(nextPageId)).build();
        return uri;
    }

    private long nextCutoff() {
        final long next = trsOrderId.incrementAndGet();
        return next;
    }

    private UriBuilder getUriBuilder() {
        return UriBuilder.fromUri(uriBase);
    }

    private URI createUuidUrn() {
        return URI.create(String.format("urn:uuid:%s", UUID.randomUUID().toString()));
    }
}
