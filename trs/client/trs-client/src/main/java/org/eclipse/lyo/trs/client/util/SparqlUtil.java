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
package org.eclipse.lyo.trs.client.util;

import java.io.IOException;
import java.net.URI;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.update.UpdateExecutionFactory;
import org.apache.jena.update.UpdateFactory;
import org.apache.jena.update.UpdateProcessor;
import org.apache.jena.update.UpdateRequest;
import org.eclipse.lyo.core.trs.ChangeEvent;
import org.eclipse.lyo.core.trs.Creation;
import org.eclipse.lyo.core.trs.Deletion;
import org.eclipse.lyo.core.trs.Modification;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

// TODO Andrew@2018-02-26: why RDF4J here?

/**
 * * A utility class with static methods enabling the processing of trs tasks as
 * sparql update requests e.g. A sparql update corresponding to the removal of
 * the old statements of a resource and the addition of the statements of its
 * update representation can be returned in exchange for a TRS Modification
 * Event
 *
 * @author Omar
 *
 */
public class SparqlUtil {

  static Logger logger = LoggerFactory.getLogger(SparqlUtil.class);

  /**
   * returns the sparql query for the creation of the named graph with the
   * given name as a String
   *
   * @param namedGraphUrl
   *            name of the named graph to be created
   * @return the graph creation sparql query as a string
   */
  public static String createGraphQuery(String namedGraphUrl) {
    String query = "CREATE GRAPH <" + namedGraphUrl + ">";
    logger.debug("query for creation of graph: " + namedGraphUrl);
    logger.debug(query);
    return query;
  }

  public static String createGraphQuery(URI namedGraphUrl) {
    return createGraphQuery(namedGraphUrl.toASCIIString());
  }

  /**
   * returns the query deleting silently the graph with the given name
   *
   * @param namedGraphUrl
   *            the name of the graph to be deleted
   * @return the deletion query as a string
   */
  public static String dropGraphQuery(String namedGraphUrl) {
    String query = "DROP GRAPH <" + namedGraphUrl + ">";
    logger.debug("query for removal of graph: " + namedGraphUrl);
    logger.debug(query);
    return query;
  }

  /**
   * returns the sparql query removing all the triples in the requested graph
   * as a string
   *
   * @param namedGraphUrl
   *            graph to be emptied
   * @return the query for emptying the graph
   */
  public static String removeAllTriplesInGraphQuery(String namedGraphUrl) {
    String query =
        "WITH"
            + " "
            + "<"
            + namedGraphUrl
            + ">"
            + "\n"
            + "DELETE"
            + "\n"
            + "{?s ?p ?o }"
            + "\n"
            + "WHERE"
            + "\n"
            + "{"
            + "\n"
            + "GRAPH"
            + " "
            + "<"
            + namedGraphUrl
            + ">"
            + "\n"
            + "{"
            + "\n"
            + "?s ?p ?o"
            + "\n"
            + "}"
            + "\n"
            + "}"
            + "\n";
    logger.debug("query for removal of all triples from graph: " + namedGraphUrl);
    logger.debug(query);
    return query;
  }

  /**
   * Gets an rdf model and a named graph as arguments and returns a sparql
   * query for adding the statements of the rdf model to the named graph
   *
   * @param namedGraphUrl
   *            named graph to which the statements shall be added
   * @param jenaModel
   *            the rdf model of which statements are to be added to the named
   *            graph
   * @return the sparql update
   */
  public static String addTriplesToGraphQuery(String namedGraphUrl, Model jenaModel) {
    try {
      String nTripleRepresentation = RdfUtil.modelToNTriple(jenaModel);
      return addTriplesToGraphQuery(namedGraphUrl, nTripleRepresentation);
    } catch (IOException e) {
      logger.error("Cannot append triples from the model to the query", e);
      return null;
    }
  }

  public static String addTriplesToGraphQuery(URI namedGraphUrl, Model jenaModel) {
    return addTriplesToGraphQuery(namedGraphUrl.toASCIIString(), jenaModel);
  }

  /**
   * Gets a set of statements and a named graph as arguments and returns a
   * sparql query for adding the statements to the named graph
   *
   * @param namedGraphUrl
   *            named graph to which the statements shall be added
   * @param triples
   *            statements to be added to the named graph
   * @return the sparql update
   */
  public static String addTriplesToGraphQuery(String namedGraphUrl, String triples) {
    String query =
        "INSERT DATA"
            + "\n"
            + "{"
            + "\n"
            + "  GRAPH"
            + " "
            + "<"
            + namedGraphUrl
            + ">"
            + "\n"
            + "{"
            + "\n"
            + triples
            + "\n"
            + "}"
            + "\n"
            + "}";
    logger.debug("query for creation of triples in graph: " + namedGraphUrl);
    logger.debug(query);
    return query;
  }

  /**
   * For a change event return a sparql update reflecting the change event.
   *
   * @param changeEvent
   *            the change event for which the sparql update is created
   * @param model
   *            the rdf model corresponding to the updated representation of
   *            the changed resource if applicable ( no rdf model is needed
   *            for a deletion event)
   * @return the sparql update
   */
  public static String getChangeEventQuery(ChangeEvent changeEvent, Model model) {

    if (changeEvent instanceof Creation) {
      return getCreationEventQuery(changeEvent, model);
    } else if (changeEvent instanceof Deletion) {
      return getDeletionEventQuery(changeEvent);
    } else if (changeEvent instanceof Modification) {
      return getModificationEventQuery(changeEvent, model);
    }
    return null;
  }

  /**
   * For a modification event return a sparql update relfecting the change
   * event.
   *
   * @param changeEvent
   *            the change event for which the sparql update is created
   * @param model
   *            the rdf model corresponding to the updated representation of
   *            the changed resource
   * @return the sparql update
   */
  public static String getModificationEventQuery(ChangeEvent changeEvent, Model model) {
    String result = "";
    String changeEventTarget = changeEvent.getChanged().toString();
    String dropGraphQuery = dropGraphQuery(changeEventTarget);
    String addGraphQuery = createGraphQuery(changeEventTarget);
    String addTriplesToNamedGraphQuery = addTriplesToGraphQuery(changeEventTarget, model);

    result = result.concat(dropGraphQuery);
    result = result.concat(";" + "\n");
    result = result.concat(addGraphQuery);
    result = result.concat(";" + "\n");
    result = result.concat(addTriplesToNamedGraphQuery);

    return result;
  }

  /**
   * For a modification event return a sparql update relfecting the change
   * event.
   *
   * @param changeEvent
   *            the change event for which the sparql update is created
   * @param triples
   *            the rdf model in n triples corresponding to the updated
   *            representation of the changed resource
   * @return the sparql update
   */
  public static String getModificationEventQuery(ChangeEvent changeEvent, String triples) {

    String changeEventTarget = changeEvent.getChanged().toString();
    return getModificationEventQuery(changeEventTarget, triples);
  }

  /**
   * For a modification event target return a sparql update relfecting the
   * change event.
   *
   * @param changeEventTarget
   *            the change resource for which the sparql update is created
   * @param triples
   *            the rdf model in n triples corresponding to the updated
   *            representation of the changed resource
   * @return the sparql update
   */
  public static String getModificationEventQuery(String changeEventTarget, String triples) {
    String result = "";
    String dropGraphQuery = dropGraphQuery(changeEventTarget);
    String addGraphQuery = createGraphQuery(changeEventTarget);
    String addTriplesToNamedGraphQuery = addTriplesToGraphQuery(changeEventTarget, triples);

    result = result.concat(dropGraphQuery);
    result = result.concat(";" + "\n");
    result = result.concat(addGraphQuery);
    result = result.concat(";" + "\n");
    result = result.concat(addTriplesToNamedGraphQuery);

    return result;
  }

  /**
   * * For a creation event return a sparql update reflecting the change
   * event.
   *
   * @param changeEvent
   *            change event for which update will be created
   * @param model
   *            updated rdf representation of the changed resource
   * @return
   */
  public static String getCreationEventQuery(ChangeEvent changeEvent, Model model) {
    String result = "";

    String changeEventTarget = changeEvent.getChanged().toString();
    String addGraphQuery = createGraphQuery(changeEventTarget);
    String addTriplesToNamedGraphQuery = addTriplesToGraphQuery(changeEventTarget, model);

    result = result.concat(addGraphQuery);
    result = result.concat(";" + "\n");
    result = result.concat(addTriplesToNamedGraphQuery);

    return result;
  }

  /**
   * For a deletion event return a sparql update relfecting the change event.
   *
   * @param changeEvent
   *            the deletion change event for which the sparql update is
   *            created
   * @return the sparql update
   */
  public static String getDeletionEventQuery(ChangeEvent changeEvent) {
    String result = "";

    String changeEventTarget = changeEvent.getChanged().toString();

    String dropGraphQuery = dropGraphQuery(changeEventTarget);

    result = result.concat(dropGraphQuery);

    return result;
  }

  /**
   * create a graph creation of the graph with the specified name sparql
   * update and post it to the given service url
   *
   * @param namedGraphUrl
   *            name of the graph to be created
   * @param serviceUrl
   *            sparql update endpoint url
   */
  public static void createGraph(String namedGraphUrl, String serviceUrl) {
    UpdateRequest request = UpdateFactory.create();
    request.add(createGraphQuery(namedGraphUrl));
    UpdateProcessor processor = UpdateExecutionFactory.createRemote(request, serviceUrl);
    processor.execute();
  }

  /**
   * create a graph creation of the graph with the specified name sparql
   * update and post it to the given service url
   *
   * @param namedGraphUrl
   *            name of the graph to be deleted
   * @param serviceUrl
   *            sparql update endpoint url
   */
  public static void dropGraph(String namedGraphUrl, String serviceUrl) {
    UpdateRequest request = UpdateFactory.create();
    request.add(dropGraphQuery(namedGraphUrl));
    UpdateProcessor processor = UpdateExecutionFactory.createRemote(request, serviceUrl);
    processor.execute();
  }

  /**
   * create a spaql update adding the triples in the given rdf model to the
   * graph with the given name and send it to the specified sparql update
   * endpoint
   *
   * @param jenaModel
   *            the triples to be added to the named graph
   * @param namedGraphUrl
   *            the named graph to which the triples shall be added
   * @param serviceUrl
   *            the sparql update endpoint
   */
  public static void addTriplesToNamedGraph(
      Model jenaModel, String namedGraphUrl, String serviceUrl) {
    UpdateRequest request = UpdateFactory.create();
    request.add(addTriplesToGraphQuery(namedGraphUrl, jenaModel));
    UpdateProcessor processor = UpdateExecutionFactory.createRemote(request, serviceUrl);
    processor.execute();
  }

  /**
   * create a spaql update removing all triples from the graph with the given
   * name and send it to the specified sparql update endpoint
   *
   * @param namedGraphUrl
   *            the named graph that shall be emptied
   * @param serviceUrl
   *            the sparql update endpoint
   */
  public static void removeAllTriplesInNamedGraph(String namedGraphUrl, String serviceUrl) {
    UpdateRequest request = UpdateFactory.create();
    request.add(removeAllTriplesInGraphQuery(namedGraphUrl));
    UpdateProcessor processor = UpdateExecutionFactory.createRemote(request, serviceUrl);
    processor.execute();
  }

  /**
   * create a spaql update reflecting the given change event and send it to
   * the sparql update endpoint
   *
   * @param changeEvent
   *            the change event to be processed
   * @param model
   *            the updated representation of the changed resource if
   *            applicable
   * @param serviceUrl
   *            the sparql update endpoint
   */
  public static void processChangeEvent(ChangeEvent changeEvent, Model model, String serviceUrl) {
    String changeEventQuery = getChangeEventQuery(changeEvent, model);
    processQuery(changeEventQuery, serviceUrl);
  }

  /**
   * Send the given sparql update to the sparql update service using the jena
   * arq libraries
   *
   * @param query
   *            sparql update to be processeda
   * @param serviceUrl
   *            sparql update endpoint for processing the sparql update
   */
  public static void processQuery(String query, String serviceUrl) {
    UpdateRequest request = UpdateFactory.create();
    request.add(query);
    UpdateProcessor processor = UpdateExecutionFactory.createRemote(request, serviceUrl);
    processor.execute();
  }

  /**
   * Send the given sparql update to the sparql update service using the
   * sesame libraries
   *
   * @param query
   *            sparql update to be processeda
   * @param serviceUrl
   *            sparql update endpoint for processing the sparql update
   * @param user
   *            username for authentication if applicable
   * @param pwd
   *            password for authentication if applicable
   */
  //    static public void processQuery_sesame(String query, String serviceUrl, String user,
  // String pwd) {
  //        SPARQLRepository repo = new SPARQLRepository(serviceUrl);
  //        repo.setUsernameAndPassword(user, pwd);
  //        repo.initialize();
  //        RepositoryConnection rc = repo.getConnection();
  //        processQuery_sesame(query, rc);
  //    }

  /**
   * Send the given sparql update to the sparql update service using the
   * sesame libraries
   *
   * @param query
   *            sparql update to be processeda
   * @param conn
   *            the repository connection object holding credentials and the
   *            sparql update endpoint
   */
  //    static public void processQuery_sesame(String query, RepositoryConnection conn) {
  //        Update u = conn.prepareUpdate(query);
  //        u.execute();
  //    }

  /**
   * return the repo connection object in order to be able to use the sesame
   * client libraries
   *
   * @param queryEndpoint
   *            the sparl query endpoint
   * @param user
   *            username for authentication if applicable
   * @param pwd
   *            password for authentication if applicable
   * @return
   */
  //    public static RepositoryConnection getRepoConnection(String queryEndpoint, String user,
  // String pwd) {
  //        SPARQLRepository repo = new SPARQLRepository(queryEndpoint);
  //        if (user != null && pwd != null && !user.isEmpty() && !pwd.isEmpty()) {
  //            repo.setUsernameAndPassword(user, pwd);
  //        }
  //        repo.initialize();
  //        try {
  //            RepositoryConnection conn = repo.getConnection();
  //            if (conn == null) {
  //                logger.error("error getting sparql repo connection !");
  //            }
  //            return conn;
  //        } catch (Exception e) {
  //            logger.error("error getting sparql repo connection !", e);
  //            return null;
  //        }
  //    }

  /**
   * return the repo connection object in order to be able to use the sesame
   * client libraries
   *
   * @param queryEndpoint
   *            the sparl query endpoint
   * @param user
   *            username for authentication if applicable
   * @param pwd
   *            password for authentication if applicable
   * @return
   */
  //    public static RepositoryConnection getRepoConnection(String queryEndpoint, String
  // updateEndPoint, String user,
  //            String pwd) {
  //        SPARQLRepository repo = new SPARQLRepository(queryEndpoint, updateEndPoint);
  //        if (user != null && pwd != null && !user.isEmpty() && !pwd.isEmpty() &&
  // !user.isEmpty()) {
  //            repo.setUsernameAndPassword(user, pwd);
  //        }
  //        repo.initialize();
  //        try {
  //            RepositoryConnection conn = repo.getConnection();
  //
  //            if (conn == null) {
  //                logger.error("error getting sparql repo connection !");
  //            }
  //            return conn;
  //        } catch (Exception e) {
  //            logger.error("error getting sparql repo connection !", e);
  //            return null;
  //        }
  //    }

  /**
   * evaluate the given sparql query against the given sparql query endpoint
   *
   * @param queryEndpoint
   *            sparql query endpoint
   * @param user
   *            username for authentication if applicable
   * @param pwd
   *            password for authentication if applicable
   * @param query
   *            sparql query
   * @return the result of the querie's evaluation
   */
  //    public static TupleQueryResult evalQuery(String queryEndpoint, String user, String pwd,
  // String query) {
  //        RepositoryConnection conn = getRepoConnection(queryEndpoint, user, pwd, query);
  //        TupleQueryResult result = null;
  //        try {
  //
  //            result = conn.prepareTupleQuery(QueryLanguage.SPARQL, query).evaluate();
  //        } catch (Exception e) {
  //            logger.error("error during the execution of the query !", e);
  //        } finally {
  //            conn.close();
  //        }
  //        return result;
  //    }

  /**
   * evaluate the given sparql update using the sesame repository connection
   * object
   *
   * @param conn
   *            repo connection sesame object
   * @param sparqlQuery
   *            sparql update to evaluate
   */
  //    public static void evalUpdate(RepositoryConnection conn, String sparqlQuery) {
  //        try {
  //
  //            conn.prepareUpdate(QueryLanguage.SPARQL, sparqlQuery).execute();
  //        } catch (Exception e) {
  //            logger.error("error during the execution of the query !", e);
  //        }
  //    }

  /**
   * evaluate the given sparql query using the sesame repository connection
   * object
   *
   * @param conn
   *            repo connection sesame object
   * @param sparqlQuery
   *            sparql query to evaluate
   * @return the query's evaluation result
   */
  //    public static TupleQueryResult evalQuery(RepositoryConnection conn, String sparqlQuery) {
  //        TupleQueryResult result = null;
  //        try {
  //
  //            result = conn.prepareTupleQuery(QueryLanguage.SPARQL, sparqlQuery).evaluate();
  //
  //        } catch (Exception e) {
  //            logger.error("error during the execution of the query !", e);
  //        }
  //
  //        return result;
  //    }

  /**
   * append a sparql update to another
   *
   * @param appending
   *            the original sparql update
   * @param appended
   *            the sparql update to be appended
   * @return the concatnated sparql update
   */
  public static String appendSparqldQuery(String appending, String appended) {
    if (appending != null && !appending.isEmpty()) {
      if (!appending.endsWith(";")) {
        appending = appending.concat(";");
      }

      if (!appending.endsWith("\n")) {
        appending = appending.concat("\n");
      }
    }

    appending = appending.concat(appended);

    return appending;
  }

  /**
   * Create a sparql update ading the triples to the named graph with the
   * specified name and send it to the sparql update endpoint specified using
   * the given repo connection object. Uses the sesame libraries
   *
   * @param conn
   *            sesame repo connection object
   * @param triples
   *            triples to be added to the named graph
   * @param graphName
   *            named graph to which the triples shall be added
   */
  //    public void processTripleAdditionQuery(RepositoryConnection conn, String triples, String
  // graphName) {
  //        String addTriplesToGraphQuery = SparqlUtil.addTriplesToGraphQuery(graphName, triples);
  //        SparqlUtil.processQuery_sesame(addTriplesToGraphQuery, conn);
  //    }

  /**
   * Create a triple with the link type as a predicate the src as subject and
   * destination as object. This is a convenience for enabling the creation of
   * links in a generic way
   *
   * @param src
   *            source of the link
   * @param dst
   *            destination of the link
   * @param linkType
   *            type of the link
   * @return the rdf triple as n triple
   */
  public static String linkTriple(String src, String dst, String linkType) {
    StringBuilder sb = new StringBuilder();
    sb.append("<" + src + ">");
    sb.append(" ");
    sb.append("<" + linkType + ">");
    sb.append(" ");
    sb.append("<" + dst + ">");
    sb.append(" .");
    return sb.toString();
  }
}
