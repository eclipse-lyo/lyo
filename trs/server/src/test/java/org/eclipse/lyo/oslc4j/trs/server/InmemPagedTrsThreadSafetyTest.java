package org.eclipse.lyo.oslc4j.trs.server;

import org.eclipse.lyo.core.trs.Base;
import org.eclipse.lyo.core.trs.ChangeEvent;
import org.eclipse.lyo.core.trs.ChangeLog;
import org.junit.Assert;
import org.junit.Test;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.concurrent.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class InmemPagedTrsThreadSafetyTest {

    private List<URI> collectAllBaseMembers(InmemPagedTrs trs) {
        List<URI> allMembers = new ArrayList<>();
        Base current = trs.getBaseFirst();
        while (current != null) {
            allMembers.addAll(current.getMembers());
            current = trs.getNext(current);
        }
        return allMembers;
    }

    private List<ChangeLog> extractPages(InmemPagedTrs trs) {
        List<ChangeLog> pages = new ArrayList<>();
        ChangeLog current = trs.getChangeLogLast();

        while (current != null) {
            pages.add(current);
            current = trs.getPrevious(current);
        }

        Collections.reverse(pages);
        return pages;
    }

    private List<Base> extractBases(InmemPagedTrs trs) {
        List<Base> bases = new ArrayList<>();
        Base current = trs.getBaseFirst();
        while (current != null) {
            bases.add(current);
            current = trs.getNext(current);
        }

        Collections.reverse(bases);
        return bases;
    }

    @Test
    public void testConcurrentHistoryUpdates() throws Exception {
        int totalEvents = 37;
        int changelogPageLimit = 10;
        int basePageLimit = 3;
        int baseResources = 12;

        InmemPagedTrs trs = new InmemPagedTrs(basePageLimit, changelogPageLimit, 
                URI.create("http://localhost:1337/trs/"), 
                IntStream.range(0, baseResources).mapToObj(i -> TRSTestUtil.dummyUri()).collect(Collectors.toSet()));

        ExecutorService exec = Executors.newFixedThreadPool(8);

        for (int i = 0; i < totalEvents; i++) {
            exec.submit(() -> {
                trs.onHistoryData(TRSTestUtil.createHistory());
            });
        }

        exec.shutdown();
        exec.awaitTermination(10, TimeUnit.SECONDS);

        // ---- ASSERT BASE ----
        List<Base> bases = extractBases(trs);
        int expectedBases = (int) Math.ceil((double) baseResources / basePageLimit);
        Assert.assertEquals(expectedBases, bases.size());

        List<URI> allBaseMembers = collectAllBaseMembers(trs);
        Assert.assertEquals(baseResources, allBaseMembers.size());

        // ---- EXTRACT PAGES ----
        List<ChangeLog> pages = extractPages(trs);
        int expectedPages = (int) Math.ceil((double) totalEvents / changelogPageLimit);
        Assert.assertEquals(expectedPages, pages.size());

        // ---- ASSERT PAGE SIZES ----
        for (int i = 0; i < pages.size(); i++) {
            ChangeLog page = pages.get(i);

            int expectedSize =
                    (i < pages.size() - 1)
                            ? changelogPageLimit
                            : (totalEvents % changelogPageLimit == 0
                                ? changelogPageLimit
                                : totalEvents % changelogPageLimit);

            Assert.assertEquals(expectedSize, page.getChange().size());
        }

        // ---- ASSERT ORDERING ----
        // Optional: ensure events are present, not necessarily ordered
        Set<URI> ids = pages.stream()
                .flatMap(p -> p.getChange().stream())
                .map(ChangeEvent::getChanged)
                .collect(Collectors.toSet());

        Assert.assertEquals(totalEvents, ids.size());
    }
}
