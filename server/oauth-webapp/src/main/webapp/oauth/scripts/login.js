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
require([
  "dojo/dom",
  "dojo/dom-construct",
  "dojo/on",
  "dojo/_base/event",
  "dojo/_base/xhr",
  "dojo/ready",
], function (dom, domConstruct, on, event, xhr, ready) {
  function showError(message) {
    var errorNode = dom.byId("error");
    if (message) {
      domConstruct.empty(errorNode);
      errorNode.appendChild(document.createTextNode(message));
    } else {
      errorNode.innerHTML = "An error occurred.";
    }
    errorNode.style.display = "block";
  }

  function returnToConsumer() {
    var callback = dom.byId("callback").value;
    if (callback) {
      window.location = callback;
    } else {
      dom.byId("content").innerHTML =
        '<div class="message">Request authorized. Close the browser window to continue.</div>';
    }
  }

  function submit() {
    xhr.post({
      url: "login",
      form: "loginForm",
      headers: {
        "X-CSRF-Prevent": lyoOAuthConfig.csrfPrevent,
      },
      load: function () {
        returnToConsumer();
      },
      error: function (error, ioArgs) {
        showError(ioArgs.xhr.responseText);
      },
    });
  }

  function cancel() {
    dom.byId("content").innerHTML =
      '<div class="message">Access denied. Close the browser window to continue.</div>';
  }

  ready(function () {
    on(dom.byId("loginForm"), "submit", function (e) {
      event.stop(e);
      submit();
    });

    on(dom.byId("cancel"), "click", function (e) {
      event.stop(e);
      cancel();
    });
  });
});
