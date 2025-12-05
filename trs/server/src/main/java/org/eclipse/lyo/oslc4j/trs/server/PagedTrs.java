package org.eclipse.lyo.oslc4j.trs.server;

import java.net.URI;

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
     * Get a Base resource by its URI
     *
     * @param uri URI of the Base resource
     * @return Base resource or null if not found
     */
    Base getBaseResource(URI uri);

    /**
     * Get the first Base resource.
     *
     * @return the first Base resource, or null if none exist
     */
    Base getBaseFirst();

    /**
     * Get the Base resource following the given Base.
     *
     * @param base the current Base resource
     * @return the next Base resource, or null if current is the last page
     */
    Base getNext(Base base);


    /**
     * Get a ChangeLog page
     *
     * @param page ChangeLog page number
     * @return ChangeLog page with the populated link to the previous ChangeLog page.
     */
    ChangeLog getChangeLog(Integer page);

    /**
     * Get a ChangeLog page by its URI
     *
     * @param uri URI of the ChangeLog page
     * @return ChangeLog page or null if not found
     */
    ChangeLog getChangeLog(URI uri);

    /**
     * Get the last ChangeLog page.
     *
     * @return the last ChangeLog page
     */
    ChangeLog getChangeLogLast();

    /**
     * Get the previous ChangeLog of the given changeLog.
     *
     * @return the previous ChangeLog page
     */
    ChangeLog getPrevious(ChangeLog changeLog);

    
    /**
     * @return number of the Base pages
     */
    int basePageCount();

    /**
     * @return number of the ChangeLog pages
     */
    int changelogPageCount();

}
