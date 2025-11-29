package org.eclipse.lyo.oslc4j.trs.server;

import org.apache.commons.lang3.RandomUtils;
import org.eclipse.lyo.core.trs.ChangeEvent;
import org.eclipse.lyo.core.trs.Creation;
import org.eclipse.lyo.core.trs.Deletion;
import org.eclipse.lyo.core.trs.Modification;

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

    static ChangeEvent historyDataToChangeEvent(HistoryData objToConvert) {
        HistoryData hd = objToConvert;

        ChangeEvent changeEvent = null;

        String hdType = hd.getType();
        Date timeStamp = hd.getTimestamp();
        URI changed = hd.getUri();
        int changeOrderInt = RandomUtils.secure().randomInt();
        String changeOrder = String.valueOf(changeOrderInt);

        String changedUriString = "urn:urn-3:" + "cm1.example.com" + ":"
                + TRSUtil.getXsdDateTimeFormat().format(hd.getTimestamp()) + ":" +
                changeOrder;

        URI changedUri = URI.create(changedUriString);

        if (hdType.equals(HistoryData.CREATED)) {
            changeEvent = new Creation(changedUri, changed, changeOrderInt);
        } else if (hdType.equals(HistoryData.MODIFIED)) {
            changeEvent = new Modification(changedUri, changed, changeOrderInt);
        } else {
            changeEvent = new Deletion(changedUri, changed, changeOrderInt);
        }
        ;

        changeEvent.getExtendedProperties().put(TRSUtil.dateModifiedQname, timeStamp);
        return changeEvent;
    }
}
