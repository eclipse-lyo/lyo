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
package org.eclipse.lyo.core.query.test;

import org.eclipse.lyo.core.query.ParseException;
import org.eclipse.lyo.core.query.QueryUtils;
import org.eclipse.lyo.core.query.SearchTermsClause;
import org.junit.Test;

/**
 * Basic tests of oslc.searchTerms clause parsing
 */
public class BasicSearchTermsTest {
    @Test
    public void testSearchTerms() throws ParseException {
        String[] expressions = {"\"foobar\"", "\"foobar\",\"whatsis\\\"yousa\""};

        for (String expression : expressions) {
            SearchTermsClause stringList = QueryUtils.parseSearchTerms(expression);

            System.out.println(stringList);
        }
    }

    @Test(expected = ParseException.class)
    public void testBadSearchTerms() throws ParseException {
        QueryUtils.parseSearchTerms("");
    }
}
