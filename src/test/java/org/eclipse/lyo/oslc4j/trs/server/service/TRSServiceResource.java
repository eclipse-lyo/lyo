package org.eclipse.lyo.oslc4j.trs.server.service;

import java.net.URI;
import java.util.Date;
import java.util.UUID;
import javax.ws.rs.Path;
import org.eclipse.lyo.oslc4j.trs.server.ChangeHistories;
import org.eclipse.lyo.oslc4j.trs.server.HistoryData;
import org.eclipse.lyo.oslc4j.trs.server.SimpleChangeHistories;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Path("/trs")
public class TRSServiceResource extends TrackedResourceSetService {
    private final static Logger log = LoggerFactory.getLogger(TRSServiceResource.class);

    public TRSServiceResource() {
        log.info("Starting up");
    }

    @Override
    protected ChangeHistories getChangeHistories() {
        log.trace("Change History objects requested");
        return new SimpleChangeHistories(10) {
            @Override
            public HistoryData[] getHistory(final Date dateAfter) {
                return new HistoryData[]{
                        createHistory(),
                        createHistory(),
                        createHistory(),
                        createHistory(),
                        createHistory()
                };
            }
        };
    }

    private static HistoryData createHistory() {
        final HistoryData historyData = HistoryData.getInstance(new Date(),
                URI.create(String.format("urn:uuid:%s", UUID.randomUUID().toString())),
                HistoryData.CREATED);
        return historyData;
    }
}
