/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
 *
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the Eclipse Public License v1.0
 *  and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *  
 *  The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 *  and the Eclipse Distribution License is available at
 *  http://www.eclipse.org/org/documents/edl-v10.php.
 *  
 *  Contributors:
 *  
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
require([ "dojo/dom", "dojo/dom-construct", "dojo/dom-style", "dojo/on",
		"dojo/_base/array", "dojo/_base/xhr", "dojo/ready" ], function(dom,
		domConstruct, domStyle, on, array, xhr, ready) {
	var pending = {};
	var approved = {};

	function showError(message) {
		var errorNode = dom.byId('error');
		if (message) {
			domConstruct.empty(errorNode);
			errorNode.appendChild(document.createTextNode(message));
		} else {
			errorNode.innerHTML = 'An error occurred.';
		}
		domStyle.set(errorNode, 'display', 'block');
	}

	function clearError() {
		domStyle.set('error', 'display', 'none');
	}
	
	function isEmpty(map) {
		for (key in map) {
	        if (map.hasOwnProperty(key)) {
	        	return false;
	        }
	    }
		
		return true;
	}
	
	function showNoPending() {
		domStyle.set('pendingTable', 'display', 'none');
		domStyle.set('noPendingMessage', 'display', 'block');
	}

	function showNoApproved() {
		domStyle.set('approvedTable', 'display', 'none');
		domStyle.set('noApprovedMessage', 'display', 'block');
	}
	
	function approve(consumer) {
		var name = dom.byId('name-' + consumer.key).value;
		if (!name) {
			showError('You must give the consumer a name.');
			return;
		}
		
		xhr.post({
			url : 'consumers/' + encodeURIComponent(consumer.key),
			content : {
				provisional : 'false',
				name : name
			},
			headers : {
				'X-CSRF-Prevent': lyoOAuthConfig.csrfPrevent
			},
			load : function(response) {
				clearError();
				
				consumer.name = name;
				consumer.provisional = false;
				
				delete pending[consumer.key];
				approved[consumer.key] = consumer;
				
				// Update the page.
				domConstruct.destroy('tableRow-' + consumer.key);
				var tbody = dom.byId('approvedTable').getElementsByTagName('tbody')[0];
				createTableRow(consumer, tbody);
				
				if (isEmpty(pending)) {
					showNoPending();
				}
				
				domStyle.set('approvedTable', 'display', 'block');
				domStyle.set('noApprovedMessage', 'display', 'none');
			},
			error : function() {
				showError();
			}
		});
	}
	
	function remove(consumer) {
		if (confirm('Delete consumer ' + consumer.name + ' (' + consumer.key
				+ ')?')) {
			xhr.del({
				url : 'consumers/' + encodeURIComponent(consumer.key),
				headers : {
					'X-CSRF-Prevent': lyoOAuthConfig.csrfPrevent
				},
				load : function(response) {
					clearError();
					domConstruct.destroy('tableRow-' + consumer.key);
					if (consumer.provisional) {
						delete pending[consumer.key];
						if (isEmpty(pending)) {
							showNoPending();
						}
					} else {
						delete approved[consumer.key];
						if (isEmpty(approved)) {
							showNoApproved();
						}
					}
				},
				error : function() {
					showError();
				}
			});
		}
	}
	
	var PROVISIONAL_ACTIONS = [ {
		name : 'Approve',
		icon : '../../oauth/images/yes.gif',
		callback : approve
	}, {
		name : 'Deny',
		icon : '../../oauth/images/no.gif',
		callback : remove
	} ];

	var APPROVED_ACTIONS = [ {
		name : 'Remove',
		icon : '../../oauth/images/no.gif',
		callback : remove
	} ];

	function createActions(consumer, node) {
		var actions = (consumer.provisional) ? PROVISIONAL_ACTIONS
				: APPROVED_ACTIONS;
		array.forEach(actions, function(action) {
			var img = domConstruct.create('img', {
				'src' : action.icon,
				'title' : action.name,
				'class' : 'action',
				'alt': action.name
			}, node);
			on(img, 'click', function() {
				action.callback(consumer);
			});
		});
	}

	function createTableRow(consumer, tbody) {
		var tr = domConstruct.create('tr', {
			'id' : 'tableRow-' + consumer.key
		}, tbody);
		
		var name = domConstruct.create('td', null, tr);
		if (consumer.provisional) {
			domConstruct.create('input', {
				'type' : 'text',
				'required' : 'true',
				'id' : 'name-' + consumer.key,
				'value' : consumer.name
			}, name);
		} else {
			name.appendChild(document.createTextNode(consumer.name))
		}
		
		var key = domConstruct.create('td', null, tr);
		key.appendChild(document.createTextNode(consumer.key));
		
		var trusted = domConstruct.create('td', null, tr);
		trusted.appendChild(document.createTextNode(consumer.trusted))
		
		var actionsNode = domConstruct.create('td', {
			style : 'text-align: center;'
		}, tr);
		createActions(consumer, actionsNode);

		return tr;
	}

	function populateTable(table, consumerArray) {
		var tbody = table.getElementsByTagName('tbody')[0];
		consumerArray.sort(function(left, right) {
			if (left.name === right.name) {
				return 0;
			}
			
			return (left.name < right.name) ? -1 : 1;
		});
		array.forEach(consumerArray, function(consumer) {
			if (consumer.provisional) {
				pending[consumer.key] = consumer;
			} else {
				approved[consumer.key] = consumer;
			}
			createTableRow(consumer, tbody);
		});
		domStyle.set(table, 'display', 'block');
	}

	function showConsumers(consumers) {
		if (consumers.provisional.length === 0) {
			showNoPending();
		} else {
			populateTable(dom.byId('pendingTable'), consumers.provisional);
		}

		if (consumers.approved.length === 0) {
			showNoApproved();
		} else {
			populateTable(dom.byId('approvedTable'), consumers.approved);
		}
	}

	function load() {
		xhr.get({
			url : 'consumers',
			headers : {
				'Accept' : 'application/json'
			},
			handleAs : 'json',
			preventCache: true,
			load : showConsumers,
			error : function() {
				showError();
			}
		});
	}

	ready(function() {
		load();
	});
});
