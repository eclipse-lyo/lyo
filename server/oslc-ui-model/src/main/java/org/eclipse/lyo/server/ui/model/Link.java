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
package org.eclipse.lyo.server.ui.model;

import jakarta.json.bind.annotation.JsonbProperty;
import jakarta.json.bind.annotation.JsonbPropertyOrder;

import java.util.Objects;

@JsonbPropertyOrder({
    "link",
    "title"
})
public class Link {

    /**
     *
     * (Required)
     *
     */
    @JsonbProperty("link")
    private String link;
    /**
     *
     * (Required)
     *
     */
    @JsonbProperty("title")
    private String title;

    /**
     *
     * (Required)
     *
     */
    @JsonbProperty("link")
    public String getLink() {
        return link;
    }

    /**
     *
     * (Required)
     *
     */
    @JsonbProperty("link")
    public void setLink(String link) {
        this.link = link;
    }

    /**
     *
     * (Required)
     *
     */
    @JsonbProperty("title")
    public String getTitle() {
        return title;
    }

    /**
     *
     * (Required)
     *
     */
    @JsonbProperty("title")
    public void setTitle(String title) {
        this.title = title;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(Link.class.getName()).append('@').append(Integer.toHexString(System.identityHashCode(this))).append('[');
        sb.append("link");
        sb.append('=');
        sb.append(((this.link == null)?"<null>":this.link));
        sb.append(',');
        sb.append("title");
        sb.append('=');
        sb.append(((this.title == null)?"<null>":this.title));
        sb.append(',');
        if (sb.charAt((sb.length()- 1)) == ',') {
            sb.setCharAt((sb.length()- 1), ']');
        } else {
            sb.append(']');
        }
        return sb.toString();
    }

    @Override
    public int hashCode() {
        int result = 1;
        result = ((result* 31)+((this.link == null)? 0 :this.link.hashCode()));
        result = ((result* 31)+((this.title == null)? 0 :this.title.hashCode()));
        return result;
    }

    @Override
    public boolean equals(Object other) {
        if (other == this) {
            return true;
        }
        if ((other instanceof Link) == false) {
            return false;
        }
        Link rhs = ((Link) other);
        return ((Objects.equals(this.link, rhs.link))&&(Objects.equals(this.title, rhs.title)));
    }

}
