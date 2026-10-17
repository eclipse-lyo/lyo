package org.eclipse.lyo.oslc4j.trs.server;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.net.URI;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import org.eclipse.lyo.core.trs.ChangeLog;
import org.eclipse.lyo.oslc4j.trs.server.service.TrackedResourceSetService;
import org.junit.Test;

public class ChangeLogUriRolloverTest {

    @Test(timeout = 10_000)
    public void fetchingTrsDuringRolloverPreservesPublishedPageUri() throws Exception {
        var currentLogFetched = new CountDownLatch(1);
        var resumeRequest = new CountDownLatch(1);
        var baseUri = URI.create("http://localhost:1337/trs");
        var pageUri = URI.create("http://localhost:1337/trs/changeLog/1");

        var pagedTrs = new InmemPagedTrs(5, 1, baseUri, "base", "changeLog", Set.of()) {
            @Override
            public ChangeLog getChangeLogLast() {
                var current = super.getChangeLogLast();
                // Pause the real service after lookup, before setChangeLog can mutate the log.
                currentLogFetched.countDown();
                try {
                    assertTrue("Request was not resumed", resumeRequest.await(3, TimeUnit.SECONDS));
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    throw new AssertionError("Request interrupted while awaiting rollover", e);
                }
                return current;
            }
        };
        var service = new TrackedResourceSetService(pagedTrs, baseUri.toString());
        pagedTrs.onHistoryData(TRSTestUtil.createHistory()); // Fill the current page.

        var executor = Executors.newSingleThreadExecutor();
        try {
            var request = executor.submit(service::getTrackedResourceSet);
            assertTrue("Request did not fetch the current log",
                    currentLogFetched.await(3, TimeUnit.SECONDS));

            // Roll over on this thread while the GET still holds the old current log.
            pagedTrs.onHistoryData(TRSTestUtil.createHistory());
            var publishedPage = pagedTrs.getChangeLog(1);
            assertEquals("Rollover must first assign the published page its URI",
                    pageUri, publishedPage.getAbout());

            resumeRequest.countDown();
            var response = request.get(3, TimeUnit.SECONDS);
            assertNull("The embedded log should be anonymous", response.getChangeLog().getAbout());
            assertEquals("Building the TRS response must not erase the stored page URI",
                    pageUri, pagedTrs.getChangeLog(1).getAbout());
        } finally {
            resumeRequest.countDown();
            executor.shutdownNow();
            assertTrue("Request executor did not stop", executor.awaitTermination(1, TimeUnit.SECONDS));
        }
    }
}
