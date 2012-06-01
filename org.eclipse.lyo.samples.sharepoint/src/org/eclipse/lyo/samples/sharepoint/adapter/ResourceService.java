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
package org.eclipse.lyo.samples.sharepoint.adapter;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URLEncoder;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.ServletInputStream;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.FileItemFactory;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.apache.poi.hslf.model.Slide;
import org.apache.poi.hslf.usermodel.SlideShow;
import org.eclipse.lyo.samples.sharepoint.SharepointConnector;
import org.eclipse.lyo.samples.sharepoint.common.IAmConstants;
import org.eclipse.lyo.samples.sharepoint.core.IConstants;
import org.eclipse.lyo.samples.sharepoint.exceptions.ConnectionException;
import org.eclipse.lyo.samples.sharepoint.exceptions.SharepointException;
import org.eclipse.lyo.samples.sharepoint.l10n.Messages;
import org.eclipse.lyo.samples.sharepoint.services.ShareBaseService;
import org.eclipse.lyo.samples.sharepoint.services.ShareServiceException;
import org.eclipse.lyo.samples.sharepoint.store.JsonFormatter2;
import org.eclipse.lyo.samples.sharepoint.store.OslcResource;
import org.eclipse.lyo.samples.sharepoint.store.ShareServerException;
import org.eclipse.lyo.samples.sharepoint.store.ShareStatement;
import org.eclipse.lyo.samples.sharepoint.store.ShareStore;
import org.eclipse.lyo.samples.sharepoint.store.ShareValue;
import org.eclipse.lyo.samples.sharepoint.store.UnrecognizedValueTypeException;
import org.eclipse.lyo.samples.sharepoint.store.XmlFormatter;
import org.eclipse.lyo.samples.sharepoint.store.JsonFormatter2.IMultiValueResolver;
import org.eclipse.lyo.samples.sharepoint.store.ShareValue.ShareValueType;
import org.eclipse.lyo.samples.sharepoint.util.StringUtils;
import org.eclipse.lyo.samples.sharepoint.util.XmlUtils;




public class ResourceService extends ShareBaseService {

	private static final long serialVersionUID = -5280734755943517104L;

	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		try {
			//ShareStore store = this.getStore();
			
			//System.out.println("ResourceService: entered" );
			
			URI reqUri = URI.create(request.getRequestURI());
			String path = reqUri.getPath();
			//System.out.println("ResourceService: entered; path='" + path +"'" );
			StringTokenizer st = new StringTokenizer(path,"/");

			if(st.countTokens() > 2 ) { 
				final SharepointConnector sc = SharepointInitializer.getSharepointConnector();
				SharepointResource resource = sc.getDocumentProperties(request.getRequestURL().toString());
				
				//System.out.println("return from getDocumentResource: " + resource.getIdentifier());
				
				if( resource == null ) {
					throw new ShareServiceException(IConstants.SC_NOT_FOUND);
				}
				
				//System.out.println("return from getDocumentResource: " + resource.getTitle());
				
				String accept = request.getHeader(IConstants.HDR_ACCEPT);
				if( this.willAccept(IConstants.CT_HTML, request) ) {
					SharepointResource sharepointResource = new SharepointResource(resource);
					request.setAttribute("resource", sharepointResource); //$NON-NLS-1$
					if( resource.isRdfType(IAmConstants.RIO_AM_PPT_DECK) ) {
						RequestDispatcher rd = request.getRequestDispatcher("/resource_deck.jsp"); //$NON-NLS-1$
						rd.forward(request, response);
						return;
					} else if( resource.isRdfType(IAmConstants.RIO_AM_PPT_SLIDE) ) {
						RequestDispatcher rd = request.getRequestDispatcher("/resource_slide.jsp"); //$NON-NLS-1$
						rd.forward(request, response);
						return;
					}
					// Try to put Sharepoints View for this document
					//kaw RequestDispatcher rd = request.getRequestDispatcher("../../sharepoint/resource_view.jsp"); //$NON-NLS-1$
					//kaw rd.forward(request, response);
				    String[] tokens = path.split("/");
					String showDocument=SharepointInitializer.getSharepointNormalUri() +
					                "/" + tokens[3] +
					                "/" + "Forms" +
					                "/" + "DispForm.aspx" +
					                "?" + "ID=" + tokens[4] +
					                "&" + "IsDlg=1";
					
					
					response.sendRedirect(showDocument);
				} else if( this.willAccept(IConstants.CT_APP_N_TRIPLES, request) ) {
					response.setContentType(IConstants.CT_JSON); 
					response.getWriter().write(resource.dumpNTriples()); 
					response.setStatus(IConstants.SC_OK);
				} else if( this.willAccept(IConstants.CT_JSON, request) ) {
					JsonFormatter2 formatter = new JsonFormatter2(new IMultiValueResolver() {
						@Override
						public boolean isMultiValued(String property) {
							if( IConstants.DCTERMS_CREATOR.equals(property) || 
									IConstants.DCTERMS_CONTRIBUTOR.equals(property) ||
									IConstants.OSLC_SERVICEPROVIDER.equals(property) ) {
								return true;
							}  
							return false;
						}
					});
					formatter.addNamespacePrefix(IAmConstants.OSLC_AM_NAMESPACE, IAmConstants.OSLC_AM_PREFIX);
					String json = formatter.format(resource);
					response.setContentType(IConstants.CT_JSON); 
					response.getWriter().write(json); 
					response.setStatus(IConstants.SC_OK);
				} else if( accept == null || this.willAccept(IConstants.CT_RDF_XML, request) || this.willAccept(IConstants.CT_XML, request)) {
					String content = XmlFormatter.formatResource(resource, IAmConstants.OSLC_AM_TYPE_RESOURCE); 
					response.setHeader(IConstants.HDR_ETAG, resource.getETag());
					String lm = StringUtils.rfc2822(resource.getModified());
					response.setHeader(IConstants.HDR_LAST_MODIFIED, lm);
					response.setStatus(IConstants.SC_OK);
					response.setContentType(IConstants.CT_RDF_XML);
					response.setContentLength(content.getBytes().length);
					response.getWriter().write(content);
				} else  if( this.willAccept(IConstants.CT_OSLC_COMPACT, request) ) {
					String content = compactDocument(resource);
					//System.out.println(content);
					response.setContentType(IConstants.CT_OSLC_COMPACT);
					response.setContentLength(content.getBytes().length);
					response.setStatus(IConstants.SC_OK);
					response.getWriter().write(content);
				} else {
					throw new ShareServiceException(IConstants.SC_UNSUPPORTED_MEDIA_TYPE, Messages.getString("Resource.UnableToAccept") + accept ); //$NON-NLS-1$
				}
			} else {
				System.out.println("OOPs -- processQuery invoked");
				//processQuery(request, response);

			}
		} catch (ShareServerException e) {
			throw new ShareServiceException(IConstants.SC_INTERNAL_ERROR, e);
		} catch (ConnectionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (SharepointException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (UnrecognizedValueTypeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	} 

	/**
	 * @see HttpServlet#doPut(HttpServletRequest, HttpServletResponse)
	 */
	protected void doPut(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		try {
			ShareStore store = this.getStore();
			OslcResource resource = store.getOslcResource(request.getRequestURL().toString());
			if( resource == null ) {
				throw new ShareServiceException(IConstants.SC_NOT_FOUND);
			}
			
			checkConditionalHeaders(request, resource);

			// cache the created and creator
			Date created = resource.getCreated();
			String creator = resource.getCreator();
			
			// ok, then we update this resource
			String contentType = request.getContentType();
			if( !contentType.startsWith(IConstants.CT_RDF_XML) ) {
				throw new ShareServiceException(IConstants.SC_UNSUPPORTED_MEDIA_TYPE);
			}
			
			ServletInputStream content = request.getInputStream();
			OslcResource updatedResource = new OslcResource(resource.getUri());
			List<ShareStatement> statements = store.parse(resource.getUri(), content, contentType);
			updatedResource.addStatements(statements);
			updatedResource.setCreated(created);
			updatedResource.setCreator(creator);
			String userId = request.getRemoteUser();
			String userUri = this.getUserUri(userId);
			store.update(updatedResource, userUri);
			
			updatedResource = store.getOslcResource(resource.getUri());
			response.setStatus(IConstants.SC_OK);
			response.addHeader(IConstants.HDR_ETAG, updatedResource.getETag());
			response.addHeader(IConstants.HDR_LOCATION, updatedResource.getUri());
			String lastModified = StringUtils.rfc2822(updatedResource.getModified());
			response.addHeader(IConstants.HDR_LAST_MODIFIED, lastModified);
			
		} catch (ShareServerException e) {
			throw new ShareServiceException(IConstants.SC_BAD, e);
		}
	}

	/**
	 * @see HttpServlet#doDelete(HttpServletRequest, HttpServletResponse)
	 */
	protected void doDelete(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		try {
			ShareStore store = this.getStore();
			OslcResource resource = store.getOslcResource(request.getRequestURL().toString());
			if( resource == null ) {
				throw new ShareServiceException(IConstants.SC_NOT_FOUND);
			}
			checkConditionalHeaders(request, resource);
			store.remove(resource); 
		} catch (ShareServerException e) {
			throw new ShareServiceException(IConstants.SC_INTERNAL_ERROR, e);
		}
	}

	@SuppressWarnings("nls")
	private String compactDocument(OslcResource resource ) throws ShareServiceException {
		String title = XmlUtils.encode(resource.getTitle());
		String shortTitle = "Resource " + resource.getIdentifier();
		String resourceUri = resource.getUri();
		try{
			String resUri = URLEncoder.encode(resourceUri, IConstants.TEXT_ENCODING);
			String iconUrl = SharepointInitializer.getBaseUri() + "/oslc.png";
			String smUrl = SharepointInitializer.getBaseUri() + "/compact/resource?uri=" + resUri + "&amp;type=small";
			String lgUrl = SharepointInitializer.getBaseUri() + "/compact/resource?uri=" + resUri + "&amp;type=large";
			String doc = 
				"<?xml version=\"1.0\" encoding=\"UTF-8\"?> \n" +
				"<rdf:RDF \n" +
				"   xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" \n" +
				"   xmlns:dcterms=\"http://purl.org/dc/terms/\" \n" +
				"   xmlns:oslc=\"http://open-services.net/ns/core#\"> \n" +
				" <oslc:Compact \n" +
				"   rdf:about=\"" + resourceUri + "\"> \n" +
				"   <dcterms:title>" + title + "</dcterms:title> \n" +
				"   <oslc:shortTitle>" + shortTitle + "</oslc:shortTitle> \n" +
				"   <oslc:icon rdf:resource=\"" + iconUrl + "\" /> \n" +
				"   <oslc:smallPreview> \n" +
				"      <oslc:Preview> \n" +
				"         <oslc:document rdf:resource=\"" + smUrl + "\" /> \n" +
				"         <oslc:hintWidth>500px</oslc:hintWidth> \n" +
				"         <oslc:hintHeight>150px</oslc:hintHeight> \n" +
				"      </oslc:Preview> \n" +
				"   </oslc:smallPreview> \n" +
				"   <oslc:largePreview> \n" +
				"      <oslc:Preview> \n" +
				"         <oslc:document rdf:resource=\"" + lgUrl + "\" /> \n" +
				"         <oslc:hintWidth>500px</oslc:hintWidth> \n" +
				"         <oslc:hintHeight>500px</oslc:hintHeight> \n" +
				"      </oslc:Preview> \n" +
				"   </oslc:largePreview> \n" +
				" </oslc:Compact> \n" +
				"</rdf:RDF>";
			return doc;
		} catch( UnsupportedEncodingException ex ) {
			throw new ShareServiceException(ex);
		}
	}
	
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		System.out.println("entered do Post for /resource");
		boolean isFileUpload = ServletFileUpload.isMultipartContent(request);
		String contentType = request.getContentType();
		
		if( !isFileUpload && !IConstants.CT_RDF_XML.equals(contentType) ) {
			throw new ShareServiceException(IConstants.SC_UNSUPPORTED_MEDIA_TYPE);
		}
		
		InputStream content = request.getInputStream();
		
		if( isFileUpload ) {
			// being uploaded from a web page
			try{
				FileItemFactory factory = new DiskFileItemFactory();
				ServletFileUpload upload = new ServletFileUpload(factory);
				@SuppressWarnings("unchecked")
				List<FileItem> items = upload.parseRequest(request);
				
				// find the first (and only) file resource in the post
				Iterator<FileItem> iter = items.iterator();
				while (iter.hasNext()) {
				    FileItem item = iter.next();
				    if (item.isFormField()) {
				        // this is a form field, maybe we can accept a title or descr?
				    } else {
				    	content = item.getInputStream(); 
				    	contentType = item.getContentType();
				    }
				}
				
			} catch( Exception e ) {
				throw new ShareServiceException(e);
			}
		}
		
		ShareStore store = this.getStore();
		if( ShareStore.rdfFormatFromContentType(contentType) != null ) {
			try {
				String resUri = store.nextAvailableUri(IAmConstants.SERVICE_RESOURCE);
				SharepointResource resource = new SharepointResource(resUri);
				List<ShareStatement> statements = store.parse(resUri, content, contentType);
				resource.addStatements(statements);
				String userUri = getUserUri(request.getRemoteUser());

				// if it parsed, then add it to the store.
				store.update(resource, userUri);
				
				// now get it back, to find 
				OslcResource returnedResource = store.getOslcResource(resource.getUri());
				Date created = returnedResource.getCreated();
				String eTag = returnedResource.getETag();
				
				response.setStatus(IConstants.SC_CREATED);
				response.setHeader(IConstants.HDR_LOCATION, resource.getUri());
				response.setHeader(IConstants.HDR_LAST_MODIFIED, StringUtils.rfc2822(created) );
				response.setHeader(IConstants.HDR_ETAG, eTag );

			} catch (ShareServerException e) {
				throw new ShareServiceException(IConstants.SC_BAD, e);
			}
		} else if( IAmConstants.CT_APP_X_VND_MSPPT.equals(contentType) || isFileUpload )  { 
			try {
				
				ByteArrayInputStream bais = isToBais(content);
				
				String uri = store.nextAvailableUri(IAmConstants.SERVICE_RESOURCE);
				SharepointResource resource = new SharepointResource(uri);
				resource.addRdfType(IAmConstants.OSLC_AM_TYPE_RESOURCE);
				resource.addRdfType(IAmConstants.RIO_AM_PPT_DECK);
				String id = resource.getIdentifier();
				String deckTitle = "PPT Deck " + id ;
				resource.setTitle(deckTitle);
				resource.setDescription("A Power Point Deck");
				String sourceUri = getBaseUrl() + '/' + IAmConstants.SERVICE_SOURCE + '/' + id; 
				resource.setSource(sourceUri);
				resource.setSourceContentType(contentType);
				String userUri = getUserUri(request.getRemoteUser());
				
				store.storeBinaryResource(bais, id);
				bais.reset();
				
				SlideShow ppt = new SlideShow(bais);
				Dimension pgsize = ppt.getPageSize();

				Slide[] slide = ppt.getSlides();
				for (int i = 0; i < slide.length; i++) {
					String slideTitle = extractTitle( slide[i] );
					String slideUri = store.nextAvailableUri(IAmConstants.SERVICE_RESOURCE);
					SharepointResource slideResource = new SharepointResource(slideUri);
					slideResource.addRdfType(IAmConstants.OSLC_AM_TYPE_RESOURCE);
					slideResource.addRdfType(IAmConstants.RIO_AM_PPT_SLIDE);
					String slideId = slideResource.getIdentifier();
					slideResource.setTitle(slideTitle);
					sourceUri = getBaseUrl() + '/' + IAmConstants.SERVICE_SOURCE + '/' + slideId; 
					slideResource.setSource(sourceUri);
					slideResource.setSourceContentType(IConstants.CT_IMAGE_PNG);
					store.update(slideResource, userUri);

					BufferedImage img = new BufferedImage(pgsize.width, pgsize.height, BufferedImage.TYPE_INT_RGB);
					Graphics2D graphics = img.createGraphics();
					graphics.setPaint(Color.white);
					graphics.fill(new Rectangle2D.Float(0, 0, pgsize.width, pgsize.height));
					slide[i].draw(graphics);
					ByteArrayOutputStream out = new ByteArrayOutputStream();
					javax.imageio.ImageIO.write(img, "png", out);
					ByteArrayInputStream is = new ByteArrayInputStream(out.toByteArray());
					store.storeBinaryResource(is, slideId );
					out.close();
					is.close();
					try{
						ShareValue v = new ShareValue(ShareValueType.URI, slideResource.getUri());
						resource.appendToSeq(IConstants.SHARE_NAMESPACE + "slides", v);
					} catch( UnrecognizedValueTypeException e) {
						// log this?  don't want to throw away everything, since this should never happen
					}
				}
				
				store.update(resource, userUri);

				// now get it back, to find eTag and creator stuff
				OslcResource returnedResource = store.getOslcResource(resource.getUri());
				Date created = returnedResource.getCreated();
				String eTag = returnedResource.getETag();
				
				response.setStatus(IConstants.SC_CREATED);
				response.setHeader(IConstants.HDR_LOCATION, resource.getUri());
				response.setHeader(IConstants.HDR_LAST_MODIFIED, StringUtils.rfc2822(created) );
				response.setHeader(IConstants.HDR_ETAG, eTag );

			} catch (ShareServerException e) {
				throw new ShareServiceException(IConstants.SC_BAD, e);
			}

		} else {
			// must be a binary or unknown format, treat as black box
			// normally a service provider will understand this and parse it appropriately
			// however this server will accept any blank box resource
			
			try {
				String uri = store.nextAvailableUri(IAmConstants.SERVICE_RESOURCE);
				SharepointResource resource = new SharepointResource(uri);
				String id = resource.getIdentifier();
				resource.setTitle("Resource " + id );
				resource.setDescription("A binary resource");
				String sourceUri = getBaseUrl() + IAmConstants.SERVICE_SOURCE + '/' + id; 
				resource.setSource(sourceUri);
				resource.setSourceContentType(contentType);
				String userUri = getUserUri(request.getRemoteUser());
				store.update(resource, userUri);
				
				store.storeBinaryResource(content, id);
				
				// now get it back, to find eTag and creator stuff
				OslcResource returnedResource = store.getOslcResource(resource.getUri());
				Date created = returnedResource.getCreated();
				String eTag = returnedResource.getETag();
				
				response.setStatus(IConstants.SC_CREATED);
				response.setHeader(IConstants.HDR_LOCATION, resource.getUri());
				response.setHeader(IConstants.HDR_LAST_MODIFIED, StringUtils.rfc2822(created) );
				response.setHeader(IConstants.HDR_ETAG, eTag );

			} catch (ShareServerException e) {
				throw new ShareServiceException(IConstants.SC_BAD, e);
			}
		}
	}
	
	public static ByteArrayInputStream isToBais(InputStream is) throws IOException {
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
//		byte[] buf = new byte[1024];
//		int len = is.read(buf);
//		while( (len = is.read(buf)) > 0 ) {
//			baos.write(buf, 0, len); 
//		}
		int b = is.read();
		while( b>=0 ) {
			baos.write(b);
			b = is.read();
		}
		is.close();
		return new ByteArrayInputStream(baos.toByteArray());
		
	}

	private String filterCharacters(String str){
		if( str == null || str.length() == 0 ) return "";
		StringBuffer inBuf = new StringBuffer(str);
		StringBuffer outBuf = new StringBuffer();
		int len = inBuf.length();
		for(int i=0;i<len;i++){
			char ch = inBuf.charAt(i);
			if( Character.isWhitespace(ch) ) {
				outBuf.append(' ');
			} else if( Character.isLetterOrDigit(ch) || ch =='\'' ) {
				outBuf.append(ch);
			}
		}
		return outBuf.toString();
	}
	
	private String extractTitle(Slide slide) {
		String title = slide.getTitle();
		if( title == null ) {
			title = "(untitled)";
		} else {
			title = filterCharacters(title);
		}
		return title;
	}	
	
/*	private void processQuery(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		
		try{
			String prefix = req.getParameter("oslc.prefix"); //$NON-NLS-1$
			String select = req.getParameter("oslc.select"); //$NON-NLS-1$
			String where = req.getParameter("oslc.where"); //$NON-NLS-1$
			String orderBy = req.getParameter("oslc.orderBy"); //$NON-NLS-1$
			String searchTerms = req.getParameter("oslc.searchTerms"); //$NON-NLS-1$
			
			SimpleQueryBuilder queryBuilder = new SimpleQueryBuilder();
			queryBuilder.parsePrefix(prefix);
			queryBuilder.parseSelect(select);
			queryBuilder.parseWhere("uri", where); //$NON-NLS-1$
			queryBuilder.parseSelect(orderBy);
			queryBuilder.parseSearchTerms(searchTerms);
			
			String sparql = queryBuilder.getQueryString(IAmConstants.OSLC_AM_TYPE_RESOURCE);
			
			ShareStore store = this.getStore();
			String queryUri = req.getRequestURL().toString();
			
			List<Map<String, ShareValue>> results = store.query(IConstants.SPARQL, sparql, 100);
			
			// TODO: create propnames
			Map<String, PName> propNames = queryBuilder.getPropertyNames();
			
			String responseResource = buildResponseResource(results, propNames, queryUri);
			resp.setContentType(IConstants.CT_RDF_XML); 
			resp.getWriter().write(responseResource); 
			resp.setStatus(IConstants.SC_OK);
			
		} catch( Exception e ) {
			throw new ShareServiceException(IConstants.SC_INTERNAL_ERROR, e);
		}
	}*/
	
	/*private String buildResponseResource(List<Map<String, ShareValue>> results, Map<String, PName> propNames, String reqUri) throws RioServiceException{
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setValidating(true);
		factory.setNamespaceAware(true);
		try{
			DocumentBuilder builder = factory.newDocumentBuilder();
			Document doc = builder.newDocument();
			Element rdf = doc.createElementNS(IConstants.RDF_NAMESPACE, IConstants.RDF_TYPE_PTERM_RDF);
			doc.appendChild(rdf);
			
			Set<String> namespaces = namespacePrefixes.keySet();
			for (String namespace : namespaces) {
				rdf.setAttribute("xmlns:" + namespacePrefixes.get(namespace), namespace); //$NON-NLS-1$
			}
			
			Element responseElement = doc.createElementNS(IConstants.OSLC_NAMESPACE, IConstants.OSLC_PTERM_RESPONSEINFO);
			responseElement.setAttributeNS(IConstants.RDF_NAMESPACE, IConstants.RDF_PTERM_ABOUT, reqUri);
			rdf.appendChild(responseElement);
			
			Element title = doc.createElementNS(IConstants.DCTERMS_NAMESPACE, IConstants.DCTERMS_PTERM_TITLE);
			responseElement.appendChild(title);
			title.setTextContent(Messages.getString("ResourceQuery.Title"));
			
			Element rdfDescr = doc.createElementNS(IConstants.RDF_NAMESPACE, IConstants.RDF_TYPE_PTERM_DESCRIPTION);
			rdf.appendChild(rdfDescr);
			rdfDescr.setAttributeNS(IConstants.RDF_NAMESPACE, IConstants.RDF_PTERM_ABOUT, reqUri);
			
			Iterator<Map<String, ShareValue>> iterator = results.iterator();
			while( iterator.hasNext() ) {
				
				Element rdfMem = doc.createElementNS(IConstants.RDFS_NAMESPACE, IConstants.RDFS_PTERM_MEMBER);
				rdfDescr.appendChild(rdfMem);
				Map<String, ShareValue> map = iterator.next();
				ShareValue uri = map.get("uri"); //$NON-NLS-1$
				rdfMem.setAttributeNS(IConstants.RDF_NAMESPACE, IConstants.RDF_PTERM_RESOURCE, uri.stringValue());
			}
				
			return XmlUtils.prettyPrint(doc);
			
		} catch (ParserConfigurationException e) {
			throw new ShareServiceException(e);
		} finally {
			
		}
	}*/
	
	static private Map<String,String> namespacePrefixes = new HashMap<String,String>();
	static  {
		namespacePrefixes.put(IConstants.RDF_NAMESPACE, IConstants.RDF_PREFIX);
		namespacePrefixes.put(IConstants.RDFS_NAMESPACE, IConstants.RDFS_PREFIX);
		namespacePrefixes.put(IConstants.OSLC_NAMESPACE, IConstants.OSLC_PREFIX);
		namespacePrefixes.put(IConstants.DCTERMS_NAMESPACE, IConstants.DCTERMS_PREFIX);
	}	
	
}
 