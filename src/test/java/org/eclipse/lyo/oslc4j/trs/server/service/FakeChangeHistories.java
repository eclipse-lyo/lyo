package org.eclipse.lyo.oslc4j.trs.server.service;

import java.net.URI;
import java.util.Date;
import java.util.List;
import org.eclipse.lyo.core.trs.Base;
import org.eclipse.lyo.core.trs.ChangeLog;
import org.eclipse.lyo.core.trs.Page;
import org.eclipse.lyo.oslc4j.trs.server.IChangeHistories;
import org.eclipse.lyo.oslc4j.trs.server.HistoryData;

public class FakeChangeHistories implements IChangeHistories {
    @Override
    public HistoryData[] getHistory(final Date dateAfter) {
        return new HistoryData[0];
    }

    @Override
    public void updateHistories(final List<HistoryData> hd) {

    }

    @Override
    public HistoryData[] getOrderedHistory(final Date dateAfter) {
        return new HistoryData[0];
    }

    @Override
    public Base getBaseResource(final String page) {
        final Base base = new Base();
        base.setAbout(URI.create("http://localhost:9998/trs/base"));
        final Page nextPage = new Page();
        nextPage.setAbout(URI.create("http://localhost:9998/trs/base/1"));
        final Page nextNextPage = new Page();
        nextNextPage.setAbout(URI.create("http://localhost:9998/trs/base/2"));

        base.setNextPage(nextPage);

        nextPage.setPageOf(base);
        nextPage.setNextPage(nextNextPage.getAbout());

        nextPage.setPageOf(base);

        return base;
    }

    @Override
    public ChangeLog getChangeLog(final String page) {
        return null;
    }
}
