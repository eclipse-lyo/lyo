/*
 * Copyright (c) 2026 Contributors to the Eclipse Foundation
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
package org.eclipse.lyo.server.oauth.core.utils;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URISyntaxException;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

import org.eclipse.lyo.server.oauth.core.OAuthConfiguration;
import org.eclipse.lyo.server.oauth.core.consumer.AbstractConsumerStore;
import org.eclipse.lyo.server.oauth.core.consumer.ConsumerStoreException;
import org.eclipse.lyo.server.oauth.core.consumer.LyoOAuthConsumer;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import net.oauth.OAuth;
import net.oauth.OAuthException;
import net.oauth.OAuthMessage;
import net.oauth.OAuthProblemException;
import net.oauth.OAuthValidator;
import net.oauth.SimpleOAuthValidator;

class AbstractAdapterCredentialsFilterTest {

    private static final String CONSUMER_KEY = "provisional-client";

    private final TestConsumerStore consumerStore = new TestConsumerStore();
    private final TestFilter filter = new TestFilter(consumerStore);

    @BeforeEach
    void setUp() throws ConsumerStoreException {
        OAuthConfiguration configuration = OAuthConfiguration.getInstance();
        configuration.setConsumerStore(consumerStore);
        configuration.setValidator(new AcceptingOAuthValidator());
    }

    @AfterEach
    void tearDown() throws ConsumerStoreException {
        OAuthConfiguration configuration = OAuthConfiguration.getInstance();
        configuration.setConsumerStore(null);
        configuration.setValidator(new SimpleOAuthValidator());
    }

    @Test
    void provisionalTrustedConsumerCannotUseTwoLeggedOAuth() throws Exception {
        LyoOAuthConsumer consumer = consumer(true, true);
        consumerStore.addConsumer(consumer);

        assertTwoLeggedOAuthRejected();
    }

    @Test
    void unknownConsumerCannotUseTwoLeggedOAuth() throws Exception {
        assertTwoLeggedOAuthRejected();
    }

    @Test
    void untrustedApprovedConsumerCannotUseTwoLeggedOAuth() throws Exception {
        LyoOAuthConsumer consumer = consumer(false, false);
        consumerStore.addConsumer(consumer);

        assertTwoLeggedOAuthRejected();
    }

    @Test
    void approvedTrustedConsumerCanUseTwoLeggedOAuth() throws Exception {
        LyoOAuthConsumer consumer = consumer(false, true);
        consumerStore.addConsumer(consumer);

        assertDoesNotThrow(this::validateTwoLeggedOAuthMessage);
    }

    private void assertTwoLeggedOAuthRejected() throws Exception {
        OAuthProblemException exception = assertThrows(OAuthProblemException.class,
                this::validateTwoLeggedOAuthMessage);

        assertEquals(OAuth.Problems.TOKEN_REJECTED, exception.getProblem());
    }

    private void validateTwoLeggedOAuthMessage() throws Exception {
        Method method = AbstractAdapterCredentialsFilter.class
                .getDeclaredMethod("validateTwoLeggedOAuthMessage", OAuthMessage.class);
        method.setAccessible(true);
        try {
            method.invoke(filter, twoLeggedMessage());
        } catch (InvocationTargetException exception) {
            Throwable cause = exception.getCause();
            if (cause instanceof Exception checkedException) {
                throw checkedException;
            }
            if (cause instanceof Error error) {
                throw error;
            }
            throw exception;
        }
    }

    private static LyoOAuthConsumer consumer(boolean provisional, boolean trusted) {
        LyoOAuthConsumer consumer = new LyoOAuthConsumer(CONSUMER_KEY, "secret");
        consumer.setProvisional(provisional);
        consumer.setTrusted(trusted);
        return consumer;
    }

    private static OAuthMessage twoLeggedMessage() {
        return new OAuthMessage(OAuthMessage.GET, "https://example.com/services/resource",
                OAuth.newList(OAuth.OAUTH_CONSUMER_KEY, CONSUMER_KEY,
                        OAuth.OAUTH_TOKEN, ""));
    }

    private static final class TestConsumerStore extends AbstractConsumerStore {
        @Override
        public LyoOAuthConsumer addConsumer(LyoOAuthConsumer consumer) {
            return add(consumer);
        }

        @Override
        public LyoOAuthConsumer removeConsumer(String consumerKey) {
            return remove(consumerKey);
        }

        @Override
        public LyoOAuthConsumer updateConsumer(LyoOAuthConsumer consumer) {
            return add(consumer);
        }

        @Override
        public void closeConsumerStore() {
            // Nothing to close.
        }
    }

    private static final class AcceptingOAuthValidator implements OAuthValidator {
        @Override
        public void validateMessage(OAuthMessage message, net.oauth.OAuthAccessor accessor)
                throws OAuthException, IOException, URISyntaxException {
            // The test targets the consumer approval decision, not signature validation.
        }
    }

    private static final class TestFilter extends AbstractAdapterCredentialsFilter<Void, Void> {
        private final TestConsumerStore consumerStore;

        private TestFilter(TestConsumerStore consumerStore) {
            super("test", "test");
            this.consumerStore = consumerStore;
        }

        @Override
        protected Void getCredentialsFromRequest(HttpServletRequest request) {
            return null;
        }

        @Override
        protected Void getCredentialsForOAuth(String id, String password) {
            return null;
        }

        @Override
        protected Void login(Void credentials, HttpServletRequest request)
                throws UnauthorizedException, ServletException {
            return null;
        }

        @Override
        protected boolean isAdminSession(String id, Void session, HttpServletRequest request) {
            return false;
        }

        @Override
        protected org.eclipse.lyo.server.oauth.core.consumer.ConsumerStore createConsumerStore() {
            return consumerStore;
        }

        @Override
        protected void logout(Void loginSession, HttpSession session) {
            // Nothing to close.
        }
    }
}
