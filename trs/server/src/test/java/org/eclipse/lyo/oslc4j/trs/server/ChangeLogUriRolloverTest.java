package org.eclipse.lyo.oslc4j.trs.server;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.net.URI;
import java.util.Set;
import org.eclipse.lyo.core.trs.TrackedResourceSet;
import org.junit.Test;

public class ChangeLogUriRolloverTest {

    @Test(timeout = 10_000)
    public void embeddingFetchedLogAfterRolloverPreservesPublishedPageUri() throws Exception {
        var baseUri = URI.create("http://localhost:1337/trs");
        var pageUri = URI.create("http://localhost:1337/trs/changeLog/1");
        var pagedTrs = new InmemPagedTrs(5, 1, baseUri, "base", "changeLog", Set.of());
        pagedTrs.onHistoryData(TRSTestUtil.createHistory()); // Fill the current page.

        // Model the interleaving in TrackedResourceSetService.getTrackedResourceSet():
        // the request fetches the current log, then a writer rolls it over before
        // the request passes that same log to TrackedResourceSet.setChangeLog().
        // Execute those operations explicitly so no scheduler timing is required.
        var fetchedLog = pagedTrs.getChangeLogLast();
        assertNull("The current log starts anonymous", fetchedLog.getAbout());

        pagedTrs.onHistoryData(TRSTestUtil.createHistory());
        assertEquals("Rollover must first assign the published page its URI",
                pageUri, pagedTrs.getChangeLog(1).getAbout());

        var response = new TrackedResourceSet();
        response.setChangeLog(fetchedLog);
        assertNull("The embedded log should be anonymous", response.getChangeLog().getAbout());
        assertEquals("Building the TRS response must not erase the stored page URI",
                pageUri, pagedTrs.getChangeLog(1).getAbout());
    }
}
