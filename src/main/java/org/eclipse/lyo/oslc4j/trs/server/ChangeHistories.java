package org.eclipse.lyo.oslc4j.trs.server;

import java.util.Date;
import java.util.List;
import javax.servlet.http.HttpServletRequest;
import org.eclipse.lyo.core.trs.Base;
import org.eclipse.lyo.core.trs.ChangeLog;

public interface ChangeHistories {
    /**
     * Implemented by inheriting classes. returns the changes through time to the resources exposes
     * by the adapter starting from the given point in time
     *
     * @param dateAfter the date starting from which the change data is returned
     */
    HistoryData[] getHistory(Date dateAfter);

    void updateHistories(List<HistoryData> hd);

    /**
     * Order the history data returned by the getHistory method to use it in the rest of the class
     */
    HistoryData[] getOrderedHistory(Date dateAfter);

    /**
     * Return page's Base resource
     *
     * @param page            the required page of the base
     * @return the required page of the base
     */
    Base getBaseResource(String page);

    /**
     * Return page's ChangeLog
     *
     * @param page            the required page of the change log
     * @return the required page of the change log
     */
    ChangeLog getChangeLog(String page);
}
