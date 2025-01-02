package org.eclipse.lyo.oslc4j.trs.server;


import java.math.BigInteger;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import org.eclipse.lyo.core.trs.Base;
import org.eclipse.lyo.core.trs.ChangeEvent;
import org.junit.Test;

import static org.assertj.core.api.Assertions.*;

public class InmemPagedTrsTest {

    @Test
    public void testEmptyLog() {
        final InmemPagedTrs inmemPagedTrs = buildPagedTrs();

        assertThat(inmemPagedTrs.basePageCount()).isEqualTo(1);

        final Base base = inmemPagedTrs.getBaseResource(1);
        assertThat(base.getAbout()).hasPath("/trs/base");
        assertThat(base.getNextPage().getAbout()).hasPath("/trs/base/1");
        assertThat(base.getMembers()).hasSize(0);
    }

    @Test
    public void testEmptyLogWithBase() {
        final InmemPagedTrs inmemPagedTrs = buildPagedTrs(Set.of(TRSTestUtil.dummyUri(), TRSTestUtil
                .dummyUri()));

        final Base base = inmemPagedTrs.getBaseResource(1);
        assertThat(base.getMembers()).hasSize(2);
    }

    @Test
    public void testEmptyLogWithPagedBase() {
        final InmemPagedTrs inmemPagedTrs = buildPagedTrs(
                Set.of(TRSTestUtil.dummyUri(), TRSTestUtil.dummyUri(), TRSTestUtil.dummyUri(), TRSTestUtil
                                .dummyUri(), TRSTestUtil.dummyUri(),
                        TRSTestUtil.dummyUri(), TRSTestUtil.dummyUri()));

        assertThat(inmemPagedTrs.basePageCount()).isEqualTo(2);

        final Base base1 = inmemPagedTrs.getBaseResource(1);
        assertThat(base1.getMembers()).hasSize(5);

        final Base base2 = inmemPagedTrs.getBaseResource(2);
        assertThat(base2.getMembers()).hasSize(2);
    }

    @Test
    public void testLogWithEmptyBase() {
        final InmemPagedTrs pagedTrs = buildPagedTrs(Set.of());

        pagedTrs.onHistoryData(TRSTestUtil.createHistory());
        pagedTrs.onHistoryData(TRSTestUtil.createHistory());
        pagedTrs.onHistoryData(TRSTestUtil.createHistory());

        assertThat(pagedTrs.changelogPageCount()).isEqualTo(1);

        assertThat(pagedTrs.getChangeLog(1).getChange()).hasSize(3);
    }

    @Test
    public void testPagedLogWithEmptyBase() {
        final InmemPagedTrs pagedTrs = buildPagedTrs(Set.of());

        pagedTrs.onHistoryData(TRSTestUtil.createHistory());
        pagedTrs.onHistoryData(TRSTestUtil.createHistory());
        pagedTrs.onHistoryData(TRSTestUtil.createHistory());
        pagedTrs.onHistoryData(TRSTestUtil.createHistory());
        pagedTrs.onHistoryData(TRSTestUtil.createHistory());

        pagedTrs.onHistoryData(TRSTestUtil.createHistory());
        pagedTrs.onHistoryData(TRSTestUtil.createHistory());

        assertThat(pagedTrs.changelogPageCount()).isEqualTo(2);

        assertThat(pagedTrs.getChangeLog(1).getChange()).hasSize(5);
        assertThat(pagedTrs.getChangeLog(2).getChange()).hasSize(2);
    }

    @Test
    public void testLogOrderUnique() {
        final InmemPagedTrs pagedTrs = buildPagedTrs(Set.of());

        pagedTrs.onHistoryData(TRSTestUtil.createHistory());
        pagedTrs.onHistoryData(TRSTestUtil.createHistory());
        pagedTrs.onHistoryData(TRSTestUtil.createHistory());
        pagedTrs.onHistoryData(TRSTestUtil.createHistory());
        pagedTrs.onHistoryData(TRSTestUtil.createHistory());

        pagedTrs.onHistoryData(TRSTestUtil.createHistory());
        pagedTrs.onHistoryData(TRSTestUtil.createHistory());

        assertThat(pagedTrs.getChangeLog(1)
                .getChange()
                .stream()
                .map(e -> e.getOrder())
                .collect(Collectors.toSet())).hasSize(5);
        assertThat(pagedTrs.getChangeLog(2)
                .getChange()
                .stream()
                .map(e -> e.getOrder())
                .collect(Collectors.toSet())).hasSize(2);
    }

    @Test
    public void testLogOrderMonotonic() {
        final InmemPagedTrs pagedTrs = buildPagedTrs(Set.of());

        pagedTrs.onHistoryData(TRSTestUtil.createHistory());
        pagedTrs.onHistoryData(TRSTestUtil.createHistory());
        pagedTrs.onHistoryData(TRSTestUtil.createHistory());
        pagedTrs.onHistoryData(TRSTestUtil.createHistory());
        pagedTrs.onHistoryData(TRSTestUtil.createHistory());

        pagedTrs.onHistoryData(TRSTestUtil.createHistory());
        pagedTrs.onHistoryData(TRSTestUtil.createHistory());

        final Optional<BigInteger> firstPageOrderMax = pagedTrs.getChangeLog(1)
                .getChange()
                .stream()
                .map(ChangeEvent::getOrder)
                .max(BigInteger::compareTo);
        assertThat(pagedTrs.getChangeLog(2)
                .getChange()
                .stream()
                .filter(e -> e.getOrder().compareTo(firstPageOrderMax.get()) > 0)
                .collect(Collectors.toSet())).hasSize(2);
    }

    @Test
    public void testLogPagesLinked() {
        final InmemPagedTrs pagedTrs = buildPagedTrs(Set.of());

        pagedTrs.onHistoryData(TRSTestUtil.createHistory());
        pagedTrs.onHistoryData(TRSTestUtil.createHistory());
        pagedTrs.onHistoryData(TRSTestUtil.createHistory());
        pagedTrs.onHistoryData(TRSTestUtil.createHistory());
        pagedTrs.onHistoryData(TRSTestUtil.createHistory());

        pagedTrs.onHistoryData(TRSTestUtil.createHistory());
        pagedTrs.onHistoryData(TRSTestUtil.createHistory());

        assertThat(pagedTrs.getChangeLog(2).getPrevious()).isEqualTo(pagedTrs.getChangeLog(1).getAbout());
    }

    @Test
    public void testLogPagesLinkedFirstNil() {
        final InmemPagedTrs pagedTrs = buildPagedTrs(Set.of());

        pagedTrs.onHistoryData(TRSTestUtil.createHistory());
        pagedTrs.onHistoryData(TRSTestUtil.createHistory());
        pagedTrs.onHistoryData(TRSTestUtil.createHistory());
        pagedTrs.onHistoryData(TRSTestUtil.createHistory());
        pagedTrs.onHistoryData(TRSTestUtil.createHistory());

        pagedTrs.onHistoryData(TRSTestUtil.createHistory());
        pagedTrs.onHistoryData(TRSTestUtil.createHistory());

        assertThat(pagedTrs.getChangeLog(1).getPrevious()).isEqualTo(TRSUtil.NIL_URI);
    }

    private InmemPagedTrs buildPagedTrs() {
        return new InmemPagedTrs(5, 5, URI.create("http://localhost:1337/trs/"),
                new ArrayList<>(0));
    }

    private InmemPagedTrs buildPagedTrs(final Collection<URI> baseUris) {
        return new InmemPagedTrs(5, 5, URI.create("http://localhost:1337/trs/"), baseUris);
    }

}
