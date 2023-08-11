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
package org.eclipse.lyo.oslc4j.provider.jena;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Writer;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.jena.rdf.model.*;
import org.apache.jena.rdf.model.impl.Util;
import org.apache.jena.util.CharEncoding;
import org.apache.jena.util.FileUtils;
import org.apache.jena.util.iterator.ExtendedIterator;
import org.apache.jena.vocabulary.RDF;

/**
 * <p>RDF writer or serializer in the abbreviated XML syntax.</p>
 *
 * <p>Supported features include:</p>
 *
 * <ul>
 * <li>Reification serialization compliant with the <a href="http://open-services.net/bin/view/Main/QmSpecificationV2#Labels_for_Relationships">OSLC Quality Management V 2.0 Specification (Labels for Relationships)</a>
 * guidance.</li>
 * <li>Properties:
 * <ul>
 * <li>{@link #RDF_PROPERTY_SHOW_XML_DECLARATION}</li>
 * <li>{@link #RDF_PROPERTY_INDENT}</li>
 * <li>{@link #RDF_PROPERTY_TAB}</li>
 * </ul>
 * </li>
 * </ul>
 *
 *
 * @author	Patrick Streule
 * @author	Paul Slauenwhite
 * @author	Martin Aeschlimann
 * @author	Sandeep Somavarapu
 * @since	1.0
 * @see		RDFWriterI
 */
@SuppressWarnings("FieldCanBeLocal")
public class RdfXmlAbbreviatedWriter implements RDFWriterI {

	private final Map<AnonId, String> resourceIdToShortIdMap;
	private int shortIdCounter = 0;
	private Boolean showXmlDeclaration = null;
	private int indent = 0;
	private int tab = 2;
	private RDFErrorHandler errorHandler = null;

	/**
	 * <p>If <code>false</code>, an XML declaration is not included with the serialized RDF model.
	 * If <code>true</code>, the XML declaration without the encoding is included with the serialized
	 * RDF model.  If not set (default) or <code>true</code> and the {@link java.io.OutputStreamWriter} uses an
	 * encoding other than UTF-8/UTF-16, the XML declaration with the encoding is included with the
	 * serialized RDF model.<p>
	 */
	private static String RDF_PROPERTY_SHOW_XML_DECLARATION = "showXmlDeclaration"; //$NON-NLS-1$

	/**
	 * <p>Number of indent spaces of the serialized RDF model (default: 0).</p>
	 *
	 * <p>Note: This property in only supported when using the abbreviated XML
	 * syntax.</p>
	 */
	private static String RDF_PROPERTY_INDENT = "indent"; //$NON-NLS-1$

	/**
	 * <p>Number of tab spaces of the serialized RDF model (default: 2).</p>
	 */
	private static String RDF_PROPERTY_TAB = "tab"; //$NON-NLS-1$

	//RDF (elements):
	private static String RDF_ELEMENT_RDF = "RDF"; //$NON-NLS-1$
	private static String RDF_ELEMENT_DESCRIPTION = "Description"; //$NON-NLS-1$

	//RDF (attributes):
	private static String RDF_ATTRIBUTE_ABOUT = "about"; //$NON-NLS-1$
	private static String RDF_ATTRIBUTE_ID = "ID"; //$NON-NLS-1$
	private static String RDF_ATTRIBUTE_RESOURCE = "resource"; //$NON-NLS-1$
	private static String RDF_ATTRIBUTE_LANG = "lang"; //$NON-NLS-1$
	private static String RDF_ATTRIBUTE_PARSE_TYPE = "parseType"; //$NON-NLS-1$
	private static String RDF_ATTRIBUTE_DATATYPE = "datatype"; //$NON-NLS-1$

	//RDF (constants):
	private static String RDF_CONSTANT_LITERAL = "Literal"; //$NON-NLS-1$
	private static String RDF_CONSTANT_RESOURCE = "Resource"; //$NON-NLS-1$

	private static final Logger logger = Logger.getLogger(RdfXmlAbbreviatedWriter.class.getName());

	/**
	 * <p>The template for the XML declaration.</p>
	 *
	 * <p>Substitution tokens (<code>{&lt;zero-based token index&gt;}</code>) mappings:</p>
	 *
	 * <ul>
	 * <li>{0}: The encoding.</li>
	 * </ul>
	 *
	 * <p>For example:</p>
	 *
	 * <p><code>&lt;?xml version=\"1.0\" encoding=\"UTF-8\"?&gt;</code></p>
	 *
	 * @see MessageFormat#format(String, Object...)
	 * @see #ENCODING_UTF8
	 */
	private static String XML_DECLARATION_ENCODING = "<?xml version=\"1.0\" encoding=\"{0}\"?>"; //$NON-NLS-1$

	//Encodings:
	private static String ENCODING_UTF8 = "UTF-8"; //$NON-NLS-1$
	private static String ENCODING_UTF16 = "UTF-16"; //$NON-NLS-1$

	//Namespaces:
	private static String NAMESPACE_URI_RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"; //$NON-NLS-1$
	private static String NAMESPACE_URI_XML = "http://www.w3.org/XML/1998/namespace"; //$NON-NLS-1$

	//Prefixes:
	private static String PREFIX_XMLNS = "xmlns"; //$NON-NLS-1$
	private static String PREFIX_RDF = "rdf"; //$NON-NLS-1$

	public RdfXmlAbbreviatedWriter() {
		resourceIdToShortIdMap = new HashMap<>();
	}

	/**
	 * <p>Sets a property on this writer.</p>
	 *
	 * <p>Supported properties include:</p>
	 *
	 * <ul>
	 * <li>{@link #RDF_PROPERTY_SHOW_XML_DECLARATION}</li>
	 * <li>{@link #RDF_PROPERTY_INDENT}</li>
	 * <li>{@link #RDF_PROPERTY_TAB}</li>
	 * </ul>
	 *
	 * @param propName The (supported) property name.
	 * @param propValue The property value ({@link String}, {@link Boolean}, or {@link Integer}), otherwise <code>null</code>.
	 * @return The old property value, otherwise <code>null</code>.
	 * @see org.apache.jena.rdf.model.RDFWriterI#setProperty(java.lang.String, java.lang.Object)
	 */
	public Object setProperty(String propName, Object propValue) {

		String oldPropertyValue = null;

		if(RDF_PROPERTY_SHOW_XML_DECLARATION.equals(propName)){

			if(showXmlDeclaration != null){
				oldPropertyValue = String.valueOf(showXmlDeclaration);
			}

			if(propValue instanceof Boolean){
				showXmlDeclaration = ((Boolean)(propValue));
			}
			else if(propValue instanceof String){
				showXmlDeclaration = Boolean.valueOf(((String)(propValue)));
			}
		}
		else if(RDF_PROPERTY_TAB.equals(propName)){

			oldPropertyValue = String.valueOf(tab);

			if(propValue instanceof Integer){
				tab = (Integer) (propValue);
			}
			else if(propValue instanceof String){

				try {
					tab = Integer.parseInt(((String) (propValue)));
				}
				catch (NumberFormatException n) {

					if(errorHandler != null){
						errorHandler.warning(n);
					}
				}
			}
		}
		else if(RDF_PROPERTY_INDENT.equals(propName)){

			oldPropertyValue = String.valueOf(indent);

			if(propValue instanceof Integer){
				indent = (Integer) (propValue);
			}
			else if(propValue instanceof String){

				try {
					indent = Integer.parseInt(((String) (propValue)));
				}
				catch (NumberFormatException n) {

					if(errorHandler != null){
						errorHandler.warning(n);
					}
				}
			}
		}

		return oldPropertyValue;
	}

	/* (non-Javadoc)
	 * @see org.apache.jena.rdf.model.RDFWriterI#setErrorHandler(org.apache.jena.rdf.model.RDFErrorHandler)
	 */
	public RDFErrorHandler setErrorHandler(RDFErrorHandler errorHandler) {

		RDFErrorHandler oldErrorHandler = this.errorHandler;

		this.errorHandler = errorHandler;

		return oldErrorHandler;
	}

	/* (non-Javadoc)
	 * @see org.apache.jena.rdf.model.RDFWriterI#write(org.apache.jena.rdf.model.Model, java.io.OutputStream, java.lang.String)
	 */
	public void write(Model model, OutputStream out, String base) {
		write(model, FileUtils.asUTF8(out), base);
	}

	/* (non-Javadoc)
	 * @see org.apache.jena.rdf.model.RDFWriterI#write(org.apache.jena.rdf.model.Model, java.io.Writer, java.lang.String)
	 */
	public void write(Model model, Writer writer, String base) {

		//Note: The base parameter is not required.

		//Serialize the RDF model:
		XMLWriter xmlWriter = new XMLWriter(writer, model, indent, tab);

		//Note: XML declaration is shown when a) encoding is not UTF8/UTF16 (default) or b) configured.
		if((showXmlDeclaration == null) || (Boolean.TRUE.equals(showXmlDeclaration))){

			String encoding = null;

			if (writer instanceof OutputStreamWriter) {

				String writerEncoding = ((OutputStreamWriter)(writer)).getEncoding();

				if ((!writerEncoding.equals(ENCODING_UTF8)) && (!writerEncoding.equals(ENCODING_UTF16))) {
					encoding = CharEncoding.create(writerEncoding).name();
				}
			}

			if ((encoding != null) || (Boolean.TRUE.equals(showXmlDeclaration))){
				xmlWriter.xmlDeclaration(encoding);
			}
		}

		//Serialize the RDF model:
		xmlWriter.rootStartTag(RDF.getURI(), RDF_ELEMENT_RDF);

		//Resolve the objects of the model:
		List<Resource> objects = new ArrayList<>();
		//Filter the following statements:
		//1) A subject of any other statement(s) in the model (reified statement).
		//2) A literal resource as the object of the statement.
		//3) A subject, predicate, or object as the predicate of the statement.
		ExtendedIterator<Statement> statementIterator = model.listStatements().filterKeep(
				statement -> {
					Property predicate = statement.getPredicate();

					return ((!statement.isReified()) && (statement.getObject().isResource())
							&& (!RDF.subject.equals(predicate)) && (!RDF.predicate.equals(
							predicate)) && (!RDF.object.equals(predicate)));
				});

		while (statementIterator.hasNext()) {

			//Assumption: All statements have an URI or anonymous resource as the object.
			Statement stmt = statementIterator.next();
			logger.finest("Added model object: " + stmt.getSubject().toString() + " " + stmt.getPredicate().toString() + " " + stmt.getObject().toString());
			Resource resource =	 stmt.getObject().asResource();

			//Maintain uniqueness:
			if(!objects.contains(resource)) {
				objects.add(resource);
			}
		}

		//Resolve the proposed root resources of the model.	 These include:
		//1. Resources not the object of any other statements
		//2. Other non-anonymous resources
		List<Resource> rootResources = new ArrayList<>();
		statementIterator = model.listStatements();

		while (statementIterator.hasNext()) {

			Resource subject = statementIterator.next().getSubject();

			if( ! (objects.contains(subject) && subject.isAnon()) ){

				//Maintain uniqueness:
				if(!rootResources.contains(subject)) {
					rootResources.add(subject);
					logger.finest("Added proposed root: " + subject.getURI());
				}
			}
		}

		//Test the candidate root resources to see if they are the objects of other root resource candidates.
		//If a candidate is the object of another root resource candidate, propose it for removal from root resources.
		//If all candidates are proposed for removal (i.e. fully cyclic graph), keep them all try to serialize.
		Set<Resource> removalCandidates = new HashSet<>();
		Set<Resource> rootTestAgainst = new HashSet<>();
		rootTestAgainst.addAll(rootResources);
		rootTestAgainst.addAll(objects);
		for (Resource rootCandidate: rootResources) {
			if (logger.isLoggable(Level.FINEST)) {
				logger.finest("validating root candidate: " + rootCandidate.toString());
			}
			for (Resource rootTest: rootTestAgainst){
				if (logger.isLoggable(Level.FINEST)) {
					logger.finest("testing against resource: " + rootTest.toString());
				}
				if (!rootTest.equals(rootCandidate)) {
					boolean rootCandidateIsObject = model.listStatements(rootTest, null, rootCandidate).hasNext();
					if (rootCandidateIsObject) {
						boolean objectIsReification = model.listStatements(rootTest, RDF.type, RDF.Statement).hasNext();
						if (!objectIsReification) {
							// checks if there is a self cyclic reference for the resource.
							boolean isCyclic = isChild(model, new HashSet<>(), rootCandidate, rootCandidate);
							if (!isCyclic) {
								removalCandidates.add(rootCandidate);
								if (logger.isLoggable(Level.FINEST)) {
									logger.finest("removed: " + rootCandidate.toString());
								}
							}
						}
					}
				}
			}
		}

		//Take removal candidates out of the rootResource list only if all candidates are not proposed for removal.
		if (! (removalCandidates.size() == rootResources.size())) {
			rootResources.removeAll(removalCandidates);
		}

		List<Resource> serializedResources = new ArrayList<>();

		for(Resource rootResource : rootResources){

			if(!rootResource.canAs(ReifiedStatement.class) && (!serializedResources.contains(rootResource))){

				serializedResources.add(rootResource);
				String rootResourceNameSpace = RDF.getURI();
				String rootResourceName = RDF_ELEMENT_DESCRIPTION;

				Statement rootResourceType = rootResource.getProperty(RDF.type);
				String rootResourceTypeURI = null;

				if (rootResourceType != null) {

					Resource rootResourceTypeResource = ((Resource)(rootResourceType.getObject()));

					rootResourceNameSpace = rootResourceTypeResource.getNameSpace();
					rootResourceName = rootResourceTypeResource.getLocalName();
					rootResourceTypeURI = rootResourceTypeResource.getURI();
				}

				xmlWriter.startTag(rootResourceNameSpace, rootResourceName, true);

				if(!rootResource.isAnon()){
					xmlWriter.attribute(RDF.getURI(), RDF_ATTRIBUTE_ABOUT, (Util.substituteEntitiesInElementContent(rootResource.getURI())));
				}

				xmlWriter.closeStartTag(true);

				serializeStatements(rootResource, xmlWriter, serializedResources, rootResourceTypeURI);

				xmlWriter.endTag(rootResourceNameSpace, rootResourceName, true);
			}
		}

		//Iterate and serialize all the reified statements in the model:
		RSIterator reifiedStatementsIterator = model.listReifiedStatements();

		while(reifiedStatementsIterator.hasNext()) {

			Resource reifiedStatement = reifiedStatementsIterator.next();

			xmlWriter.startTag(RDF.getURI(), RDF_ELEMENT_DESCRIPTION, true);

			if(reifiedStatement.isAnon()){
				xmlWriter.attribute(RDF.getURI(), RDF_ATTRIBUTE_ABOUT, ('#' + getShortId(reifiedStatement.getId())));
			}
			else{
				xmlWriter.attribute(RDF.getURI(), RDF_ATTRIBUTE_ABOUT, (Util.substituteEntitiesInElementContent(reifiedStatement.getURI())));
			}

			xmlWriter.closeStartTag(true);

			serializeStatements(reifiedStatement, xmlWriter, serializedResources, null);

			xmlWriter.endTag(RDF.getURI(), RDF_ELEMENT_DESCRIPTION, true);
		}

		xmlWriter.rootEndTag(RDF.getURI(), RDF_ELEMENT_RDF, true);

		xmlWriter.end();
	}

	/**
	 * Returns true if there is a path leading from <code>parent</code> to any
	 * of the resources in <code>children</code>.
	 *
	 * @param model
	 *			  model.
	 * @param visitedResources
	 *			  resource already visited during the search.
	 * @param parent
	 *			  parent resource.
	 * @param children
	 *			  children resources.
	 * @return true if there is a path.
	 */
	private boolean isChild(Model model, Set<Resource> visitedResources, Resource parent, Resource... children) {
		boolean isChild = false;
		List<Resource> newChildren = new ArrayList<>();
		outer: for (Resource child : children)
		{
			visitedResources.add(child);
			List<Statement> list = model.listStatements(null, null, child).toList();
			for (Statement stat : list)
			{
				RDFNode subject = stat.getSubject();
				if (subject.isResource())
				{
					Resource subjectAsResource = subject.asResource();
					if (subject.equals(parent))
					{
						isChild = true;
						break outer;
					}
					else {
						if (!visitedResources.contains(subjectAsResource))
						{
							newChildren.add(subjectAsResource);
						}
					}
				}
			}
		}
		if (!isChild && newChildren.size() > 0)
		{
			Resource[] childrenResources = new Resource[newChildren.size()];
			isChild = isChild(model, visitedResources, parent, (Resource[]) newChildren.toArray(childrenResources));
		}
		return isChild;
	}

	private void serializeStatements(Resource resource, XMLWriter xmlWriter, List<Resource> serializedResources, String rootResourceTypeURI){

		StmtIterator statementIterator = resource.getModel().listStatements(resource, null, ((RDFNode)(null)));
		Set<Statement> visitedStatements = new HashSet<>();

		while (statementIterator.hasNext()) {
			serializeStatement(statementIterator.next(), xmlWriter, serializedResources,visitedStatements, rootResourceTypeURI);
		}
	}

	private void serializeStatement(Statement statement, XMLWriter xmlWriter, List<Resource> serializedResources, Set<Statement> visitedStatements, String rootResourceTypeURI) {

		visitedStatements.add(statement);
		Model model = statement.getModel();
		Property predicate = statement.getPredicate();

		if ((!RDF.subject.equals(predicate)) && (!RDF.predicate.equals(predicate)) && (!RDF.object.equals(predicate))){

			RDFNode object = statement.getObject();

			if (object.isResource()) {

				Resource resource = object.asResource();

				if(RDF.type.equals(predicate)) {
					String resourceType = resource.getURI();
					if(resourceType.equals(rootResourceTypeURI)) {

						// Resource type has already been serialized. Skipping it
						return;
					}
				}

				xmlWriter.startTag(predicate.getNameSpace(), predicate.getLocalName(), true);

				if (statement.isReified()) {

					RSIterator reifiedStatementsIterator = model.listReifiedStatements(statement);

					String reifiedStatementShortId = getShortId(reifiedStatementsIterator.next().getId());

					while (reifiedStatementsIterator.hasNext()) {

						AnonId reifiedStatementId = reifiedStatementsIterator.next().getId();

						if(!resourceIdToShortIdMap.containsKey(reifiedStatementId)){
							resourceIdToShortIdMap.put(reifiedStatementId, reifiedStatementShortId);
						}
					}

					xmlWriter.attribute(RDF.getURI(), RDF_ATTRIBUTE_ID, reifiedStatementShortId);
				}

				if ((predicate.isAnon()) && (predicate.getProperty(RDF.type) == null)) {
					xmlWriter.attribute(RDF.getURI(), RDF_ATTRIBUTE_PARSE_TYPE, RDF_CONSTANT_RESOURCE);
				}

				StmtIterator statementIterator = model.listStatements(resource, null, ((RDFNode)(null)));
				Statement firstResourceStatement = null;
				List<Statement> statementList = null;

				//Need to fully retrieve next statement to test if it has been visited to generate the correct XML
				//Convert the statement iterator into a list and use it later to serialize each statement
				if (statementIterator.hasNext()) {
					statementList = statementIterator.toList();
					firstResourceStatement = statementList.get(0);
				}

				if (firstResourceStatement != null && !visitedStatements.contains(
						firstResourceStatement)) {

					xmlWriter.closeStartTag(true);

					String resourceNameSpace = RDF.getURI();
					String resourceName = RDF_ELEMENT_DESCRIPTION;

					Statement resourceType = resource.getProperty(RDF.type);
					String nestedResourceTypeURI = null;
					if (resourceType != null) {

						Resource resourceTypeResource = ((Resource)(resourceType.getObject()));

						resourceNameSpace = resourceTypeResource.getNameSpace();
						resourceName = resourceTypeResource.getLocalName();
						nestedResourceTypeURI = resourceTypeResource.getURI();
					}

					xmlWriter.startTag(resourceNameSpace, resourceName, true);

					if(!resource.isAnon()) {
						xmlWriter.attribute(RDF.getURI(), RDF_ATTRIBUTE_ABOUT, (Util.substituteEntitiesInElementContent(resource.getURI())));
					}

					xmlWriter.closeStartTag(true);

					for (Statement nextStatement : statementList) {
						serializeStatement(nextStatement, xmlWriter, serializedResources, visitedStatements, nestedResourceTypeURI);
					}

					xmlWriter.endTag(resourceNameSpace, resourceName, true);

					xmlWriter.endTag(predicate.getNameSpace(), predicate.getLocalName(), true);
				}
				else{

					if (!resource.isAnon()) {
						xmlWriter.attribute(RDF.getURI(), RDF_ATTRIBUTE_RESOURCE, (Util.substituteEntitiesInElementContent(resource.getURI())));
					}

					xmlWriter.closeEmptyStartTag();
				}
			}
			else {

				Literal literal = ((Literal)(object));

				xmlWriter.startTag(predicate.getNameSpace(), predicate.getLocalName(), true);

				String language = literal.getLanguage();

				if ((language != null) && (language.length() > 0)) {
					xmlWriter.attribute(NAMESPACE_URI_XML, RDF_ATTRIBUTE_LANG, (Util.substituteEntitiesInElementContent(language)));
				}

				String content = literal.getLexicalForm();

				if (literal.isWellFormedXML()) {

					xmlWriter.attribute(RDF.getURI(), RDF_ATTRIBUTE_PARSE_TYPE, RDF_CONSTANT_LITERAL);
					xmlWriter.closeStartTag(false);
					xmlWriter.literal(content);
				}
				else {

					String dataTypeUri = literal.getDatatypeURI();

					if (dataTypeUri != null) {
						xmlWriter.attribute(RDF.getURI(), RDF_ATTRIBUTE_DATATYPE, (Util.substituteEntitiesInElementContent(dataTypeUri)));
					}

					xmlWriter.closeStartTag(false);
					xmlWriter.literal(Util.substituteEntitiesInElementContent(content));
				}

				xmlWriter.endTag(predicate.getNameSpace(), predicate.getLocalName(), false);
			}
		}
	}

	private String getShortId(AnonId resourceId) {

		String shortId = resourceIdToShortIdMap.get(resourceId);

		if (shortId == null) {

			shortId = ('n' + String.valueOf(shortIdCounter++));

			resourceIdToShortIdMap.put(resourceId, shortId);
		}

		return shortId;
	}

	/**
	 * <p>XML writer or serializer.</p>
	 *
	 * <p>Supported features include:</p>
	 *
	 * <ul>
	 * <li>Indentation using tabs.</li>
	 * </ul>
	 *
	 *
	 * @author	Patrick Streule
	 * @author	Paul Slauenwhite
	 * @author	Martin Aeschlimann
	 * @author	Sandeep Somavarapu
	 * @version 1.0
	 * @since	1.0
	 */
	private class XMLWriter {

		private final PrintWriter printWriter;
		private final Map<String, String> namespaceMap;
		private final int indent;
		private final int tab;
		private int tabCount = 0;

		public XMLWriter(Writer writer, Model model, int indent, int tab) {

			if(writer instanceof PrintWriter){
				this.printWriter = ((PrintWriter)(writer));
			}
			else{
				this.printWriter = new PrintWriter(writer);
			}

			//Create the namespace map that maps namespaces to namespace prefixes:
			namespaceMap = new HashMap<>();

			//Resolve and add the namespace map defined in the model:
			Map<String, String> modelNamespacePrefixMap = model.getNsPrefixMap();

			for(Entry<String, String> namespaceEntry : modelNamespacePrefixMap.entrySet()){
				namespaceMap.put(namespaceEntry.getValue(), namespaceEntry.getKey());
			}

			//Resolve and add the default namespace:
			namespaceMap.put(NAMESPACE_URI_RDF, PREFIX_RDF);

			//Resolve and add the namespaces defined for the predicate and type object in each statement in the model:
			int unknownNamespacePrefixCounter = 0;
			ExtendedIterator<Statement> statementIterator = model.listStatements();

			while (statementIterator.hasNext()) {

				Statement statement = statementIterator.next();
				Property predicate = statement.getPredicate();

				if ((!RDF.subject.equals(predicate)) && (!RDF.predicate.equals(predicate)) && (!RDF.object.equals(predicate))){

					String namespace;

					if(RDF.type.equals(predicate)){
						namespace = statement.getObject().asResource().getNameSpace();
					}
					else{
						namespace = predicate.getNameSpace();
					}

					if(!namespaceMap.containsKey(namespace)){
						namespaceMap.put(namespace, "j." + (unknownNamespacePrefixCounter++)); //$NON-NLS-1$
					}
				}
			}

			this.indent = indent;
			this.tab = tab;
		}

		public void xmlDeclaration(String encoding){

			if(encoding != null){
				printWriter.print(MessageFormat.format(XML_DECLARATION_ENCODING, encoding));
			}
			else{
				printWriter.print(XML_DECLARATION_ENCODING);
			}

			printWriter.println();
		}

		public void rootStartTag(String namespaceUri, String localName){

			tab();

			tabCount++;

			startTag(namespaceUri, localName, false);

			//Serialize the namespace prefixes:
			for(Entry<String, String> namespaceEntry : namespaceMap.entrySet()){

				printWriter.println();

				tab();
				tab();

				final String buffer = PREFIX_XMLNS + ':' + namespaceEntry.getValue() + "=\"" +
						namespaceEntry.getKey() + '"';
				printWriter.print(buffer);
			}

			closeStartTag(true);

			tabCount--;
		}

		public void startTag(String namespaceUri, String localName, boolean isChildStartTag) {

			if(isChildStartTag){

				tabCount++;

				tab();
			}

			final String buffer = "<" + namespaceMap.get(namespaceUri) + ':' + localName;
			printWriter.print(buffer);
		}

		public void attribute(String namespaceUri, String localName, String value) {

			final String buffer = " " + namespaceMap.get(namespaceUri) + ':' + localName + "=\"" +
					value + '"';
			printWriter.print(buffer);
		}

		public void closeEmptyStartTag() {

			printWriter.print("/>"); //$NON-NLS-1$

			printWriter.println();

			tabCount--;
		}

		public void closeStartTag(boolean isParent) {

			printWriter.print(">"); //$NON-NLS-1$

			if(isParent){
				printWriter.println();
			}
		}

		public void literal(String literal) {
			printWriter.print(literal);
		}

		public void endTag(String namespaceUri, String localName, boolean isParent) {

			rootEndTag(namespaceUri, localName, isParent);

			tabCount--;
		}

		public void rootEndTag(String namespaceUri, String localName, boolean isParent) {

			if(isParent){
				tab();
			}

			final String buffer = "</" + //$NON-NLS-1$
					namespaceMap.get(namespaceUri) + ':' + localName + '>';
			printWriter.print(buffer);

			printWriter.println();
		}

		public void end(){
			printWriter.flush();
		}

		private void tab() {

			char[] spaces = new char[(tab * tabCount) + indent];

			if(spaces.length > 0){

				Arrays.fill(spaces, ' ');

				printWriter.print(new String(spaces));
			}
		}
	}
}
