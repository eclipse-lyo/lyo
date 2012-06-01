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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;


import org.eclipse.lyo.samples.sharepoint.core.IConstants;
import org.eclipse.lyo.samples.sharepoint.l10n.Messages;
import org.eclipse.lyo.samples.sharepoint.store.ShareValue.ShareValueType;
import org.openrdf.model.BNode;
import org.openrdf.model.Literal;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.model.ValueFactory;
import org.openrdf.query.Binding;
import org.openrdf.query.BindingSet;
import org.openrdf.query.QueryLanguage;
import org.openrdf.query.TupleQuery;
import org.openrdf.query.TupleQueryResult;
import org.openrdf.repository.Repository;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.RepositoryException;
import org.openrdf.repository.RepositoryResult;
import org.openrdf.repository.sail.SailRepository;
import org.openrdf.rio.RDFFormat;
import org.openrdf.sail.memory.MemoryStore;
import org.openrdf.sail.nativerdf.NativeStore;

public class ShareStore {
	
	static private File binResources = null;
	static private ShareStore _store = null;
	static public ShareStore initalizeStore(String repositoryLocation, String binaryResourceLocation, String host, String context) throws ShareServerException{
		if( _store != null ) {
			shutdown();
		} 

		System.out.println( Messages.getString("ShareStore.InitializingStore")); //$NON-NLS-1$
		_store = new ShareStore();
		_store.host = host;
		_store.context = context;
		File dataDir = new File(repositoryLocation);
		_store.repository = new SailRepository(new NativeStore(dataDir));
		try {
			_store.repository.initialize();
			binResources = new File(binaryResourceLocation);
			if( !binResources.exists() ) {
				binResources.mkdir();
			}
		} catch (Exception e) {
			throw new ShareServerException(e);
		}

		return _store;
	}
	
	public static boolean isStoreInitialized() {
		return (_store != null);
	}
	
	public static File getBinFolder(){
		return binResources;
	}
	
	public static ShareStore getStore() throws ShareServerException {
		if( _store == null ) {
			throw new ShareServerException(Messages.getString("ShareStore.RDFStoreNotInitialized")); //$NON-NLS-1$
		}
		return _store;
	}
	
	private ShareStore() {
	}
	
	private Repository repository = null;
	private String host = null; 
	private String context = null;
	
	synchronized private Repository getRepository() throws ShareServerException {
		if( repository == null ) {
			throw new ShareServerException("RDF Store not initialized");  //$NON-NLS-1$
		} 
		return repository;
	}

	private Repository createMemoryRepository() throws ShareServerException {
		Repository repository = new SailRepository(new MemoryStore());
		try {
			repository.initialize();
		} catch (RepositoryException e) {
			throw new ShareServerException(e);
		}
		return repository;
	}

	private int openConnections = 0;
	private RepositoryConnection getConnection() throws ShareServerException {
		try {
			openConnections++;
			System.out.println( "Open: " + openConnections ); //$NON-NLS-1$
			return getRepository().getConnection();
		} catch (RepositoryException e) {
			throw new ShareServerException(e);
		}
	}
	
	
	private void close( RepositoryConnection conn ) {
		if( conn == null ) return;
		try {
			openConnections--;
			System.out.println( "Close: " + openConnections ); //$NON-NLS-1$
			if( conn.isOpen() ) conn.close();
		} catch (RepositoryException e) {
			e.printStackTrace();
		}
	}
	
	synchronized public String nextAvailableUri(String type) throws ShareServerException {
		RepositoryConnection conn = getConnection();
		try{
			ValueFactory vf = getRepository().getValueFactory();
			URI uriCounter = vf.createURI(IConstants.URI_COUNTER);
			URI uriServer = vf.createURI(IConstants.URI_SERVER);

			int counter = -1;
			RepositoryResult<Statement> statements = conn.getStatements(uriServer, uriCounter, null, false);
			// we expect at most 1
			if( !statements.hasNext() ) {
				// create a new one
				counter = 1;
				Literal udpatedValue = vf.createLiteral(Integer.toString(counter+1), IConstants.XSD_DATATYPE_INT); // set the next value
				conn.add(uriServer, uriCounter, udpatedValue, (Resource) uriServer);
			} else {
				Statement statement = statements.next();
				Value obj = statement.getObject();
				if( obj instanceof Literal ) {
					counter = ((Literal)obj).intValue();
					
					// now remove the old and add the new
					conn.remove(statement);
					
					// add add the updated one
					Literal udpatedValue = vf.createLiteral(counter+1);
					conn.add(uriServer, uriCounter, udpatedValue, (Resource) uriServer);
				} else {
					throw new ShareServerException(Messages.getString("ShareStore.UnableToGetUriCounter"));  //$NON-NLS-1$
				}
			}
			
			return getUriBase() + '/' + type + '/' + counter; 
			
		} catch (RepositoryException e) {
			throw new ShareServerException(e);
		} finally {
			close(conn);
		}
	}
	
	public String getUriBase() {
		return host + '/' + context;
	}

	public String getServerContext() {
		return context;
	}

	/*
	 * Resource
	 */

	public OslcResource getOslcResource(String uri) throws ShareServerException {
		OslcResource oslcResource = new OslcResource(uri);
		return (OslcResource) getResource(oslcResource);
	}

	public ShareResource getResource(String uri) throws ShareServerException {
		ShareResource resource = new ShareResource(uri);
		return getResource(resource);
	}

	public ShareResource getResource(ShareResource resource) throws ShareServerException {
		String uri = resource.getUri();
		RepositoryConnection conn = getConnection();
		ValueFactory vf = conn.getValueFactory();
		URI resourceUri = vf.createURI(uri);
		// make sure there are some statements with this context
		RepositoryResult<Statement> statements = null; 
		try {
			statements = conn.getStatements(null, null, null, false, (Resource) resourceUri);
			if( statements != null && statements.hasNext() ) {
				resource.statements.clear();
				while( statements.hasNext() ) {
					Statement statement = statements.next();
					String subject = statement.getSubject().stringValue();
					String predicate = statement.getPredicate().stringValue();
					ShareValue object = convertValue(statement.getObject());
					ShareStatement rioStatement = new ShareStatement(subject, predicate, object, uri);
					resource.addStatement(rioStatement);
				}
			} else {
				resource = null;
			}
		} catch (Exception e) {
			throw new ShareServerException(e);
		} finally {
			close(conn);
		}
		
		return resource;
	}

	/**
	 * @param resource
	 * @throws ShareServerException 
	 */
	public void update(ShareResource resource, String user) throws ShareServerException {
		RepositoryConnection conn = getConnection();
		if( user == null ) {
			user = getDefaultUserUri();
		}

		try {
		
			ShareResource oldResource = null;
			if( resource instanceof OslcResource ) {
				OslcResource oslcResource = new OslcResource(resource.getResourceContext());
				oldResource = this.getResource(oslcResource);
			} else {
				oldResource = this.getResource(resource.getResourceContext());
			}
			ShareValue propCreated = null;
			ShareValue propCreator = null;
			// if has a created or created property, we need to keep and ensure it is put back
			// these are system defined properties so we can do what we want
			if( oldResource != null ) {
				if( resource instanceof OslcResource ) {
					propCreated = oldResource.getFirstPropertyValue(IConstants.DCTERMS_CREATED);
					propCreator = oldResource.getFirstPropertyValue(IConstants.DCTERMS_CREATOR);

					if( propCreator != null ) { // then add it back
						resource.setUriProperty(IConstants.DCTERMS_CREATOR, propCreator.stringValue());
					} 
					
					if( propCreated != null ) { // add it back 
						resource.setDateProperty(IConstants.DCTERMS_CREATED, propCreated.dateValue());
					} 
				}

				// remove the resource from the repo so we can update it.
				this.remove(resource);
			}
			
			// set the dc:identifier property to be the same as the last segment of this resource uri
			if( resource instanceof OslcResource ) {
				OslcResource oslcResource = (OslcResource) resource;
				String id = extractLastSegment(resource.getUri());
				oslcResource.setIdentifier(id);
				oslcResource.setContributor(user);
				oslcResource.setModified(new Date());
				if( propCreated != null ) {
					oslcResource.setCreated(propCreated.dateValue());
				} else {
					oslcResource.setCreated(new Date());
				}
				if( propCreator != null ) {
					oslcResource.setCreator(propCreator.stringValue());
				} else {
					oslcResource.setCreator(user);
				}
			}
			
			ValueFactory vf = conn.getValueFactory();
			URI resourceUri = vf.createURI(resource.getUri());  // explicitly using uri not resource context since this is a top level resource
	
			List<ShareStatement> rioStatements = resource.getStatements(); 
			
			// make sure there are some statements with this context
			for (ShareStatement rioStatement : rioStatements) {
				URI pred = vf.createURI(rioStatement.getPredicate());
				Resource subj = null;
				String rioSubject = rioStatement.getSubject();
				if( rioStatement.isBNode() ) {
					subj = vf.createBNode(rioSubject);
				} else {
					subj = vf.createURI(rioSubject);
				}
				Value val = null;
				ShareValue rioVal = rioStatement.getObject();
				if( rioVal.getType() == ShareValueType.URI ) {
					val = vf.createURI(rioVal.stringValue());
				} else if( rioVal.getType() == ShareValueType.BLANK_NODE ) {
					val = vf.createBNode(rioVal.stringValue());
				} else if( rioVal.getType() == ShareValueType.BOOLEAN ) {
					val = vf.createLiteral(rioVal.booleanValue());
				} else if( rioVal.getType() == ShareValueType.CALENDAR ) {
					val = vf.createLiteral(rioVal.xmlGregorianCalendarValue());
				} else if( rioVal.getType() == ShareValueType.DECIMAL ) {
					val = vf.createLiteral(rioVal.doubleValue());
				} else if( rioVal.getType() == ShareValueType.INTEGER ) {
					val = vf.createLiteral(rioVal.intValue());
				} else {
					val = vf.createLiteral(rioVal.stringValue());
				}
				
				Statement statement = vf.createStatement(subj, pred, val);
				conn.add(statement, (Resource) resourceUri );
			}
			
						
		} catch (Exception e) {
			throw new ShareServerException(e);
		} finally {
			close(conn);
		}
	}
	
	/**
	 * @return
	 */
	public String getDefaultUserUri() {
		return this.getUriBase() + '/' + IConstants.SHARE_UNKNOWN_USER_ID; //$NON-NLS-1$
	}

	public void storeBinaryResource(InputStream is, String id ) throws IOException {
		File file = new File(binResources, id);
		OutputStream out = new FileOutputStream(file); // Transfer bytes from in to out 
		byte[] buf = new byte[1024]; 
		int len; 
		while ((len = is.read(buf)) > 0) { 
			out.write(buf, 0, len); 
		} 
		is.close(); 
		out.close(); 
	}
	
	@SuppressWarnings({ })
	static private Map<String,ShareValueType> rdfOriTypeMap = new HashMap<String,ShareValueType>();
	static {
		rdfOriTypeMap.put(IConstants.XSD_DATATYPE_BOOLEAN, ShareValueType.BOOLEAN);
		rdfOriTypeMap.put(IConstants.XSD_DATATYPE_DECIMAL, ShareValueType.DECIMAL);
		rdfOriTypeMap.put(IConstants.XSD_DATATYPE_INT, ShareValueType.INTEGER);
		rdfOriTypeMap.put(IConstants.XSD_DATATYPE_DATETIME, ShareValueType.CALENDAR);
		rdfOriTypeMap.put("http://www.w3.org/2001/XMLSchema#dateTime", ShareValueType.CALENDAR); //$NON-NLS-1$
	}
	
	static public ShareValue convertValue(org.openrdf.model.Value rdfValue) throws UnrecognizedValueTypeException {
		if( rdfValue instanceof Literal ) {
			Literal literal = (Literal) rdfValue;
			URI datatype = literal.getDatatype();
			if( datatype != null ) {
				ShareValueType type = rdfOriTypeMap.get(datatype.stringValue());
				if( type != null ) {
					switch( type ) {
					case BOOLEAN: return new ShareValue(type, literal.booleanValue());
					case DECIMAL: return new ShareValue(type, literal.decimalValue());
					case CALENDAR: return new ShareValue(type, literal.calendarValue());
					case INTEGER: return new ShareValue(type, literal.integerValue());
					default: return new ShareValue(type, literal.toString() );
					}
				} 
			} 
			return new ShareValue(ShareValueType.STRING, rdfValue.stringValue()); // default to this type
		} else {
			// expect a uri or blank node
			// todo: might want to make uri and blank node different types
			if( rdfValue instanceof BNode ) {
				return new ShareValue(ShareValueType.BLANK_NODE, java.net.URI.create(rdfValue.stringValue()));
			} 
			if( rdfValue.stringValue().startsWith("<") ) {
				System.out.println(rdfValue.stringValue());
			}
			return new ShareValue(ShareValueType.URI, java.net.URI.create(rdfValue.stringValue()));
		}
	}
	
	static public org.openrdf.model.Value toShareValue(ValueFactory vf, ShareValue rioValue) throws UnrecognizedValueTypeException, IncompatibleValueException {
		
		switch( rioValue.getType() ) {
		case BOOLEAN: {
			Literal val = vf.createLiteral(rioValue.booleanValue());
			return val;
		}
		case INTEGER: {
			Literal val = vf.createLiteral(rioValue.intValue());
			return val;
		}
		case CALENDAR: {
			Literal val = vf.createLiteral(rioValue.xmlGregorianCalendarValue());
			return val;
		}
		case STRING: {
			Literal val = vf.createLiteral(rioValue.stringValue());
			return val;
		}
		case URI: {
			URI uri = vf.createURI(rioValue.stringValue());
			return uri;
		}
		case BLANK_NODE: {
			BNode bnode = vf.createBNode(rioValue.stringValue());
			return bnode;
		}
		default: {
			throw new UnrecognizedValueTypeException();
		}
		}
		
	}
	
	public List<ShareStatement> parse(String resUri, InputStream is, String contentType) throws ShareServerException {
		String format = rdfFormatFromContentType(contentType);
		if( format == null ) {
			throw new ShareServerException(Messages.getString("ShareStore.UnrecognizedContentType")); //$NON-NLS-1$
		}
		Repository memRepo = createMemoryRepository();
		if( resUri == null ) {
			throw new ShareServerException("Resource URI required");
		}
		
		ValueFactory vf = getRepository().getValueFactory();
		URI uri = vf.createURI(resUri);
		RepositoryConnection memConn = null;
		try {
			memConn = memRepo.getConnection();
			RDFFormat rdfFormat = RDFFormat.valueOf(format);
			memConn.add(is, resUri, rdfFormat, (Resource) uri);
			
			//  build the resource return
			List<ShareStatement> rioStatements = new ArrayList<ShareStatement>();
			RepositoryResult<Statement> statements = memConn.getStatements(null, null, null, false, (Resource) uri);
			while( statements.hasNext()) {
				Statement statement = statements.next();
				String subject = statement.getSubject().stringValue();
				String predicate = statement.getPredicate().stringValue();
				ShareValue object = convertValue(statement.getObject());
				ShareStatement rioStatement = new ShareStatement(subject, predicate, object, resUri);
				rioStatements.add(rioStatement);
			}
			return rioStatements;
			
		} catch (Exception e) {
			throw new ShareServerException(e);
		} finally {
			try {
				memConn.close();
			} catch (RepositoryException e) {
				e.printStackTrace();
			}
		}
	}	
	
	public List<ShareStatement> findStatements(String subject, String predicate, ShareValue value, String resource ) throws ShareServerException, UnrecognizedValueTypeException {
		List<ShareStatement> statements = new ArrayList<ShareStatement>();
		RepositoryConnection conn = this.getConnection();
		ValueFactory vf = conn.getValueFactory();
		Resource sub = vf.createURI(subject);
		URI pred = vf.createURI(predicate);
		RepositoryResult<Statement> results = null;

		try{
			if( resource != null ) {
				URI res = vf.createURI(resource);
				results = conn.getStatements(sub, pred, null, false, (Resource) res);
			} else {
				results = conn.getStatements(sub, pred, null, false);
			}
			while( results.hasNext() ) {
				Statement result = results.next();
				ShareStatement rioStatement = toShareStatement(result);
				statements.add(rioStatement);
			}
			return statements;
		} catch( RepositoryException re ) {
			throw new ShareServerException(re);
		}
	}
	
	
	static public ShareStatement toShareStatement(Statement statement) throws UnrecognizedValueTypeException  {
		String subject = statement.getSubject().stringValue();
		String predicate = statement.getPredicate().stringValue();
		Value obj = statement.getObject();
		ShareValue rioVal = convertValue(obj);
		String context = statement.getContext().stringValue();
		ShareStatement rioStatement = new ShareStatement(subject, predicate, rioVal, context);
		return rioStatement;
	}

	/*
	 * Query 
	 */
	
	public List<Map<String,ShareValue>> query(String queryLanguage, String query, int maxResults) throws ShareServerException {
		return query(null, queryLanguage, query, maxResults);
	}
	
	private List<Map<String,ShareValue>> query(Repository repository, String queryLanguage, String query, int maxResults) throws ShareServerException {
		if( maxResults <= 0 ) maxResults = DEFAULT_MAX_RESULTS; 
		QueryLanguage language = QueryLanguage.valueOf(queryLanguage);
		
		if( language == null ) {
			throw new ShareServerException(Messages.getString("ShareStore.UnrecognizedQueryLanguage") + queryLanguage ); //$NON-NLS-1$
		}
		
		RepositoryConnection conn = null;

		List<Map<String,ShareValue>> bindings = new ArrayList<Map<String,ShareValue>>();
		try {
			if( repository == null ) {
				conn = getConnection();
			} else {
				conn = repository.getConnection();
			}
			TupleQuery tupleQuery = conn.prepareTupleQuery(language, query);
			TupleQueryResult result = tupleQuery.evaluate();
			int count = 0;
			while( result.hasNext() && count<maxResults) {
				BindingSet bindingSet = result.next();
				HashMap<String, ShareValue> map = new HashMap<String,ShareValue>();
				
				Set<String> names = bindingSet.getBindingNames();
				for (String name : names) {
					Binding binding = bindingSet.getBinding(name);
					Value value = binding.getValue();
					ShareValue rioValue = convertValue(value);
					map.put(name, rioValue);
				}
				bindings.add(map);
			}
			
		} catch (Exception e) {
			throw new ShareServerException(e);
		} finally {
			close(conn);
		}
		return bindings;
	}
	
	
	/**
	 * @param resource
	 * @throws ShareServerException 
	 */
	public void remove(ShareResource resource) throws ShareServerException {
		RepositoryConnection conn = null;
		try {
			conn = getConnection();
			ValueFactory vf = conn.getValueFactory();
			Resource res = vf.createURI(resource.getUri());
			conn.remove((URI) null, null, null, (Resource) res);
		} catch (Exception e) {
			throw new ShareServerException(e);
		} finally {
			close(conn);
		}		
	}
	

	/**
	 * @param id
	 * @return
	 * @throws FileNotFoundException 
	 */
	public InputStream getBinaryResource(String id) throws FileNotFoundException {
		File file = new File(binResources, id);
		FileInputStream is = new FileInputStream(file);
		return is;
	}
	
	public static void shutdown() throws ShareServerException {
		if( ShareStore._store == null ) return;
		Repository repo = getStore().getRepository();
		try {
			repo.shutDown();
			ShareStore._store = null;
		} catch (RepositoryException e) {
			throw new ShareServerException(e);
		}
	}
	
	static public void dump(RepositoryConnection conn) {
		System.out.println( "Dumping Repository: "); //$NON-NLS-1$
		try{
			if( conn == null ) {
				conn = getStore().getConnection();
			}
			RepositoryResult<Statement> allStatements = conn.getStatements(null, null, null, false);
			while( allStatements.hasNext() ) {
				Statement statement = allStatements.next();
				System.out.println( statement.toString() );
			}
			allStatements.close();
		} catch( Exception e ) {
			e.printStackTrace();
		} finally {
			try{
				if( conn != null && conn.isOpen() ) conn.close();
			} catch( Exception e ) {
				e.printStackTrace(); // log this?
			}
		}
	}
	
	static public void dump( Repository repository ) throws RepositoryException{
		RepositoryConnection conn = repository.getConnection();
		dump( conn  );
	}
	
	static public void dump(RepositoryConnection conn, Writer writer) {
		try{
			writer.write( "Dumping Repository: \n" ); //$NON-NLS-1$
			if( conn == null ) {
				conn = getStore().getConnection();
			}
			
			RepositoryResult<Resource> contextIds = conn.getContextIDs();
			while( contextIds.hasNext() ) {
				Resource contextId = contextIds.next();
				writer.write( '\n' + Messages.getString("ShareStore.Resource") + contextId.toString() + '\n'); //$NON-NLS-1$

				RepositoryResult<Statement> statements = conn.getStatements(null, null, null, false, contextId);
				while( statements.hasNext() ) {
					Statement statement = statements.next();
					Resource sub = statement.getSubject();
					URI pred = statement.getPredicate();
					Value obj = statement.getObject();
					if( obj instanceof URI ) {
						writer.write( "\t<" + sub.toString() + "> <" + pred.toString() + "> <" + obj.toString() + ">\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
					} else {
						writer.write( "\t<" + sub.toString() + "> <" + pred.toString() + "> " + obj.toString() + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
					}
				}
				statements.close();
			
			}
			contextIds.close();
			
		} catch( Exception e ) {
			e.printStackTrace();
		} finally {
			try{
				if( conn != null && conn.isOpen() ) conn.close();
			} catch( Exception e ) {
				e.printStackTrace(); // log this?
			}
		}
	}
	
	public List<String> getResourceContexts(){
		List<String> resourceIds = new ArrayList<String>();
		RepositoryConnection conn = null;
		System.out.println( "Clearing Repository: "); //$NON-NLS-1$
		try{
			conn = getConnection();
			RepositoryResult<Resource> contextIds = conn.getContextIDs();
			while( contextIds.hasNext() ) {
				resourceIds.add( contextIds.next().stringValue() );
			}
		} catch( Exception e ) {
			e.printStackTrace();
		} finally {
			close(conn);
		}
		return resourceIds;
	}
	
	public void clear(String contextUri) {
		RepositoryConnection conn = null;
		System.out.println( "Clearing Repository: "); //$NON-NLS-1$
		try{
			conn = getConnection();
			ValueFactory vf = conn.getValueFactory();
			URI context = vf.createURI(contextUri);
			if( context != null ) {
				conn.clear(context);
			}
		} catch( Exception e ) {
			e.printStackTrace();
		} finally {
			close(conn);
		}
	}
	
	public void clear(Resource context) {
		RepositoryConnection conn = null;
		System.out.println( "Clearing Repository: "); //$NON-NLS-1$
		try{
			conn = getConnection();
			if( context == null ) {
				conn.clear();
			} else {
				conn.clear(context);
			}
		} catch( Exception e ) {
			e.printStackTrace();
		} finally {
			close(conn);
		}
	}
	
	public static String rdfFormatFromContentType( String contentType ) {
		int pos = contentType.indexOf(';');
		if( pos > 0 ) {
			contentType = contentType.substring(0,pos);
		}
		if( IConstants.CT_RDF_XML.equals(contentType) ){
			return RDFFormat.RDFXML.getName();
		} else if( IConstants.CT_APP_N3.equals(contentType) || IConstants.CT_TEXT_N3.equals(contentType) ){
			return RDFFormat.N3.getName();
		} else if( IConstants.CT_APP_N_TRIPLES.equals(contentType)  ){
			return RDFFormat.NTRIPLES.getName();
		} else if( IConstants.CT_TEXT_TURTLE.equals(contentType) || IConstants.CT_X_TURTLE.equals(contentType) ){
			return RDFFormat.TURTLE.getName();
		}
		return null;
	}

	static final int DEFAULT_MAX_RESULTS = 500;
	
	static private List<String> serverManagedProperties = null;

	static {
			serverManagedProperties = new ArrayList<String>();
			serverManagedProperties.add(IConstants.OSLC_SERVICEPROVIDER);
			serverManagedProperties.add(IConstants.OSLC_INSTANCESHAPE);
			serverManagedProperties.add(IConstants.DCTERMS_IDENTIFIER);
			serverManagedProperties.add(IConstants.DCTERMS_CREATED);
			serverManagedProperties.add(IConstants.DCTERMS_CREATOR);
			serverManagedProperties.add(IConstants.DCTERMS_MODIFIED);
			serverManagedProperties.add(IConstants.DCTERMS_CONTRIBUTOR);
	}
	
	static public boolean isServerManaged(URI uri) {
		return serverManagedProperties.contains(uri.stringValue());
	}

	static private Map<String,String> predefinedPrefixes = new HashMap<String,String>();
	
	static {
		predefinedPrefixes.put(IConstants.RDF_NAMESPACE, IConstants.RDF_PREFIX);
		predefinedPrefixes.put(IConstants.DCTERMS_NAMESPACE, IConstants.DCTERMS_PREFIX);
		predefinedPrefixes.put(IConstants.OSLC_NAMESPACE, IConstants.OSLC_PREFIX);
	}
	
	static public Map<String, String> getPredefinedNamespaceMappings(){
		return predefinedPrefixes;
	}
	
	@SuppressWarnings("nls")
	static public String sparqlDefaultPrefixes(){
		StringBuilder prefixes = new StringBuilder();
		Set<String> namespaces = predefinedPrefixes.keySet();
		for (String namespace : namespaces) {
			String prefix = predefinedPrefixes.get(namespace);
			prefixes.append("PREFIX " + prefix + ":<" + namespace + ">\n" );
		}
		return prefixes.toString();
	}

	public static String extractLastSegment(String uri) {
		int pos = uri.lastIndexOf('#');
		if( pos > 0 ) {
			return uri.substring(pos + 1);
		}
		pos = uri.lastIndexOf('/');
		return uri.substring(pos + 1);
	}


}
