/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *  
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *
 *     Keith Wells             - initial API and implementation
 *     Sam Padgett           - initial API and Implementation
 *     Jim Conallen           - initial API and implementation
 *
 *******************************************************************************/
package org.eclipse.lyo.samples.sharepoint.store;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.wink.json4j.JSONArray;
import org.apache.wink.json4j.JSONException;
import org.apache.wink.json4j.JSONObject;
import org.eclipse.lyo.samples.sharepoint.core.IConstants;
import org.eclipse.lyo.samples.sharepoint.store.ShareValue.ShareValueType;


public class JsonFormatter {

    private Map<String, String> namespacePrefixes = new HashMap<String, String>();
    private Map<String, JSONObject> blankNodes = new HashMap<String, JSONObject>();
//	private Map<String,Element> otherStatementIds = new HashMap<String,Element>();
//	private Map<String,Element> idElm = new HashMap<String,Element>();
    static private List<String> reifiedStatementProperties = null;

    static {
        reifiedStatementProperties = new ArrayList<String>();
        reifiedStatementProperties.add(IConstants.RDF_STATEMENT);
        reifiedStatementProperties.add(IConstants.RDF_SUBJECT);
        reifiedStatementProperties.add(IConstants.RDF_PREDICATE);
        reifiedStatementProperties.add(IConstants.RDF_OBJECT);
        reifiedStatementProperties.add(IConstants.RDF_TYPE);
    }

    public JsonFormatter() {
    }

    /*
     * Resource
     */
    static public String formatResource(ShareResource resource) throws ShareServerException {
        JsonFormatter formatter = new JsonFormatter();
        return formatter.format(resource);
    }

    public void addNamespacePrefix(String ns, String prefix) {
        this.namespacePrefixes.put(ns, prefix);
    }

    public String format(ShareResource resource) throws ShareServerException {
        initPrefixes();
        try {
            JSONObject jresource = new JSONObject();
            jresource.put("rdf:about", resource.getUri());

            List<ShareStatement> statements = resource.getStatements();

            // find all reified statements,  by looking for statement with type rdf:statement 
            Iterator<ShareStatement> i = statements.iterator();
            while (i.hasNext()) {
                ShareStatement statement = i.next();
                String predicate = statement.getPredicate();
                ShareValue object = statement.getObject();

                // String IConstants.RDF_STATEMENT IConstants.RDF_TYPE
                if (IConstants.RDF_STATEMENT.equals(object.stringValue()) && IConstants.RDF_TYPE.equals(predicate)) {
                    //createOtherDescription(statement, resource, jresource);
                }
            }

            //now get the rest of the statements
            for (ShareStatement statement : statements) {
                String subject = statement.getSubject();
                String predicate = statement.getPredicate();

                if (subject.equals(resource.getUri())) {
                    ShareValue value = statement.getObject();
                    setValue(jresource, predicate, value, resource);

                    String predObj = extractPredObj(statement);

                } else if (statement.isBNode()) {
                    if (false) {
                        ShareValue value = statement.getObject();
                    }

                } else {
                    if (!isReifiedStatementProperty(predicate)) {
                        // now put it in the right description
                        String strSubj = subject.toString();
                        String id = extractUrlFragment(strSubj);
                        if (false) {
                            String[] nsLocal = extractNsLocal(predicate);
                            String ns = nsLocal[0];
                            String local = nsLocal[1];
                            String prefix = nsLocal[2];
                            ShareValue value = statement.getObject();
                        }
                    }
                }
            }

            // set namespaces and prefixes
            setNamespaces(jresource);
            return jresource.toString(4);

        } catch (Exception e) {
            throw new ShareServerException(e);
        }
    }

    /**
     * @param predicate
     * @return
     */
    private String[] extractNsLocal(String uri) {
        int pos = uri.lastIndexOf('#');
        if (pos < 0) {
            pos = uri.lastIndexOf('/');
        }
        String namespace = uri.substring(0, pos + 1);
        String local = uri.substring(pos + 1);
        String prefix = this.getPrefix(namespace);
        return new String[]{namespace, local, prefix};
    }

    private String extractUrlFragment(String uri) {
        int pos = uri.lastIndexOf('#');
        if (pos > 0) {
            return uri.substring(pos + 1);
        }
        return null;
    }

    private String concatPredObj(String p, String o) {
        return '[' + p + ',' + o + ']';
    }

    private String extractPredObj(ShareStatement statement) {
        String p = statement.getPredicate();
        String o = statement.getObject().stringValue();
        return concatPredObj(p, o);
    }

    private void setValue(JSONObject jresource,
            String predicate, ShareValue value, ShareResource resource)
            throws JSONException, IncompatibleValueException {

        String nsLocal[] = extractNsLocal(predicate);
        String ns = nsLocal[0];
        String local = nsLocal[1];
        String prefix = nsLocal[2];
        String propertyName = prefix + ":" + local;

        if (getPrefix(ns) == null) {
            namespacePrefixes.put(ns, prefix);
        }

        JSONObject targetObject = null;
        JSONArray targetArray = null;
        
        if (jresource.containsKey(propertyName)) {
            // we have multiple values of this property

            if (jresource.get(propertyName) instanceof JSONArray) {
                // have an array, so fetch it
                targetArray = jresource.getJSONArray(propertyName);

            } else {
                // don't have an array yet, so create one
                Object firstValue = jresource.get(propertyName);
                targetArray = new JSONArray();
                targetArray.add(firstValue);
                jresource.put(propertyName, targetArray);
            }
        } else {
            targetObject = jresource;
        }

        if (value.getType() == ShareValueType.BLANK_NODE) {

            List<ShareStatement> statements = resource.getStatements(value.stringValue(), IConstants.RDF_TYPE, null);
            if (statements.size() > 0) {
                ShareStatement bNodeTypeStatement = statements.get(0);
                ShareValue bNodeType = bNodeTypeStatement.getObject();
                if (bNodeType.getType() == ShareValueType.URI) {
                    String typeUri = bNodeType.stringValue();
                    if (IConstants.RDF_SEQ.equals(typeUri)
                            || IConstants.RDF_BAG.equals(typeUri)) {

                        JSONObject bnode = new JSONObject();
                        bnode.put(local, value);
                        this.blankNodes.put(bNodeTypeStatement.getSubject(), bnode);
                    }
                }
            }

        } else if (value.getType() == ShareValueType.URI) {
            JSONObject resourceValue = new JSONObject();
            resourceValue.put("rdf:resource", value.stringValue());
            if (targetObject != null) {
                targetObject.put(propertyName, resourceValue);
            } else {
                targetArray.add(resourceValue);
            }

        } else {
            if (targetObject != null) {
                targetObject.put(propertyName, value.stringValue());
            } else {
                targetArray.add(value.stringValue());
            }
        }
    }

    private void initPrefixes() {
        namespacePrefixes.put(IConstants.RDF_NAMESPACE, IConstants.RDF_PREFIX);
        namespacePrefixes.put(IConstants.OSLC_NAMESPACE, IConstants.OSLC_PREFIX);
        namespacePrefixes.put(IConstants.DCTERMS_NAMESPACE, IConstants.DCTERMS_PREFIX);
        namespacePrefixes.put(IConstants.SHARE_NAMESPACE, IConstants.SHARE_PREFIX);
    }

    private String getPrefix(String namespace) {
        String prefix = namespacePrefixes.get(namespace);
        if (prefix == null) {
            // then find the next available prefix
            int i = 0;
            String pre = "pr" + i; //$NON-NLS-1$
            Collection<String> prefixes = namespacePrefixes.values();
            while (prefixes.contains(pre)) {
                i++;
                pre = "pr" + i; //$NON-NLS-1$
            }
            namespacePrefixes.put(namespace, pre);
            prefix = pre;
        }
        return prefix;
    }

    private void setNamespaces(JSONObject jresource) throws JSONException {
        Set<String> namespaces = namespacePrefixes.keySet();
        JSONObject prefixes = new JSONObject();
        for (String namespace : namespaces) {
            prefixes.put(namespacePrefixes.get(namespace), namespace);
        }
        jresource.put("prefixes", prefixes);
    }

    static public boolean isReifiedStatementProperty(String uri) {
        return reifiedStatementProperties.contains(uri);
    }
}
