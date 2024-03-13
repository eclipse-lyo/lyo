/*
 * Copyright (c) 2020 Contributors to the Eclipse Foundation
 *
 * See the NOTICE file(s) distributed with this work for additional
 * information regarding copyright ownership.
 *
 * This program and the accompanying materials are made available under the
 * terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0, or the Eclipse Distribution License 1.0
 * which is available at http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Clause
 */
package org.eclipse.lyo.oslc4j.trs.server;

import java.net.URI;
import java.util.Date;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class represents an event occurring on a specific resource at some point in time.
 *
 * @version $version-stub$
 * @since 2.3.0
 */
public class HistoryData {
    /**
     * trs:Creation
     */
    final public static  String CREATED  = "Created";
    /**
     * trs:Modification
     */
    final public static  String MODIFIED = "Modified";
    /**
     * trs:Deletion
     */
    final public static  String DELETED  = "Deleted";
    private final static Logger log      = LoggerFactory.getLogger(HistoryData.class);
    private Date timestamp;

    /**
     * Tracked resource URI
     */
    private URI  uri;

    private String type = CREATED;

    private HistoryData() {
    }

    @Override
    public String toString() {
        return "Type=" + getType() + ", " + "Timestamp=" + getTimestamp() + ", " + "URI=" + getUri()
                + "\n";
    }

    public static HistoryData getInstance(Date timestamp, URI uri, String type) {
        HistoryData h = new HistoryData();
        h.timestamp = timestamp;
        h.uri = uri;
        h.type = type;
        return h;
    }

    /**
     * @return the timestamp
     */
    @Deprecated
    public Date getTimestamp() {
        return timestamp;
    }

    /**
     * @return the url
     */
    public URI getUri() {
        return uri;
    }

    /**
     * @return the type
     */
    public String getType() {
        return type;
    }
}
