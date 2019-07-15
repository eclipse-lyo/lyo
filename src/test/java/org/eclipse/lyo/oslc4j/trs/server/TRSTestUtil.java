package org.eclipse.lyo.oslc4j.trs.server;

import java.net.URI;
import java.util.Date;
import java.util.Random;
import java.util.UUID;

public class TRSTestUtil {
    private static Random random = new Random();

    public static URI dummyUri() {
        return URI.create("http://localhost:1337/r/" + random.nextInt(Integer.MAX_VALUE));
    }

    public static HistoryData createHistory() {
        final HistoryData historyData = HistoryData.getInstance(new Date(),
                URI.create(String.format("urn:uuid:%s", UUID.randomUUID().toString())),
                HistoryData.CREATED);
        return historyData;
    }
}
