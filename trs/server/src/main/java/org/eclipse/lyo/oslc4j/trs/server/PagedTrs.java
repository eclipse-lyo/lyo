package org.eclipse.lyo.oslc4j.trs.server;

import org.eclipse.lyo.core.trs.Base;
import org.eclipse.lyo.core.trs.ChangeLog;

public interface PagedTrs {

    /**
     * Get a page from the paged Base
     *
     * @param page Base page number
     * @return Base resource with the populated Page
     */
    Base getBaseResource(Integer page);

    /**
     * Get a ChangeLog page
     *
     * @param page ChangeLog page number
     * @return ChangeLog page with the populated link to the previous ChangeLog page.
     */
    ChangeLog getChangeLog(Integer page);

    ChangeLog getChangeLogLast();

    /**
     * @return number of the Base pages
     */
    int basePageCount();

    /**
     * @return number of the ChangeLog pages
     */
    int changelogPageCount();

}
