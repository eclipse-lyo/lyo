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
package org.eclipse.lyo.samples.sharepoint;

import java.io.BufferedReader;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletResponse;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;


//import jbugz.exceptions.BugzillaException;
import com.sun.jersey.api.client.config.ClientConfig;


import org.apache.commons.fileupload.FileItem;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpStatus;
import org.apache.commons.httpclient.methods.InputStreamRequestEntity;
import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.commons.httpclient.methods.RequestEntity;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.eclipse.lyo.samples.sharepoint.adapter.SharepointInitializer;
import org.eclipse.lyo.samples.sharepoint.adapter.SharepointResource;
import org.eclipse.lyo.samples.sharepoint.adapter.URLStrategy;
import org.eclipse.lyo.samples.sharepoint.core.IConstants;
import org.eclipse.lyo.samples.sharepoint.exceptions.ConnectionException;
import org.eclipse.lyo.samples.sharepoint.store.ShareServerException;
import org.eclipse.lyo.samples.sharepoint.store.ShareValue;
import org.eclipse.lyo.samples.sharepoint.store.UnrecognizedValueTypeException;
import org.eclipse.lyo.samples.sharepoint.store.ShareValue.ShareValueType;
import org.joda.time.LocalDateTime;

import org.odata4j.consumer.ODataClientRequest;
import org.odata4j.consumer.ODataConsumer;
import org.odata4j.core.OClientBehavior;
import org.odata4j.core.OEntity;
import org.odata4j.core.OProperty;
import org.odata4j.core.ORelatedEntityLink;
import org.odata4j.repack.org.apache.commons.codec.binary.Base64;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;


/**
 * The <code>SharepointConnector</code> class handles all access to a given Sharepoint installation.
 * Sharepoint uses OData
 * 
 *
 */
public class SharepointConnector {

	/**
	 * Logger for internal errors.
	 */
	private static final Log logger = LogFactory.getLog("SharepointConnector.class");
	
	private ODataConsumer odatac;
	
	/**
	 * Use this method to designate a host to connect to. You must call this method 
	 * before executing any other methods of this object.
	 * 
	 * @param host A string pointing to the domain of the Sharepoint installation
	 * @throws ConnectionException if a connection cannot be established
	 */
    public void connectTo(String host) throws ConnectionException {
        connectTo(host,null,null);
    }

    /**
     * Use this method to designate a host to connect to. You must call this method 
     * before executing any other methods of this object.
     * 
     * If httpUser is not null, than the httpUser and the httpPasswd will be 
     * used to connect to the sharepoint server. This currently only supports basic
     * http authtentication ( @see <a href="http://en.wikipedia.org/wiki/Basic_access_authentication">Basic access authentication</a>).
     * 
     * @param host A string pointing to the domain of the Bugzilla installation
     * @param httpUser username for an optional Basic access authentication
     * @param httpPasswd password for an optional Basic access authentication
     * @throws ConnectionException if a connection cannot be established
     */
    public void connectTo(final String host, final String httpUser, final String httpPasswd) throws ConnectionException {
     
        URL hostURL;
        try {
            hostURL = new URL(host);
        } catch (MalformedURLException e) {
            logger.error("URL parameter for host is improperly formed; cannot connect", e);
            throw new ConnectionException("Host URL is malformed; URL supplied was " + host, e);
        }
        connectTo(hostURL,httpUser,httpPasswd);
    }

    /**
     * Use this method to designate a host to connect to. You must call this method 
     * before executing any other methods of this object.
     * 
     * If httpUser is not null, than the httpUser and the httpPasswd will be 
     * used to connect to the sharepoint server. This currently only supports basic
     * http authtentication ( @see <a href="http://en.wikipedia.org/wiki/Basic_access_authentication">Basic access authentication</a>).
     *
     * @param host A URL of form http:// + somedomain + /_vti_bin/listdata.svc
     * @param httpUser username for an optional Basic access authentication
     * @param httpPasswd password for an optional Basic access authentication
     * 
     */
    public void connectTo(URL host, String httpUser, String httpPasswd) {
    	odatac = ODataConsumer.create(
    			SharepointInitializer.getSharepointUri(),
				new OClientBehavior() {
					@Override
					public ODataClientRequest transform(
							ODataClientRequest request) {
						String userPassword = SharepointInitializer.getUsername() + ":" + SharepointInitializer.getPassword();
						String encoded = Base64.encodeBase64String(userPassword
								.getBytes());
						encoded = encoded.replaceAll("\r\n?", "");
						return request.header("Authorization", "Basic "
								+ encoded);
					}

					@Override
					public void modify(ClientConfig arg0) {
						// TODO Auto-generated method stub

					}
    			});
	}
	
    /*
     *  Get the document library collections from sharepoint
     */
    public List<Library> getLibraries() {
    	List<Library> libraries = new ArrayList<Library>();
    	HashMap<String, Object> map; 

    	// get the different collections from Sharepoint
		for(String entitySet : odatac.getEntitySets()){
		   try {
			  // ignore ReportingTemplates collection, it returns documents we are not interested in
			   if (!entitySet.startsWith("ReportingTemplates")){			      	
				   //see if there is a document in the collection, if so let's save it for the catalog 
		           for(OEntity oe1 : odatac.getEntities(entitySet).filter("ContentType eq 'Document'").top(1).execute()) {
		              map = new HashMap<String,Object>();
				      map.put("uri", SharepointInitializer.getBaseUri() + '/' + "provider?collection=" + entitySet);
				      map.put("name", entitySet);
				      libraries.add(new Library(map));
		           }		          	      
			    }
			} catch(java.lang.RuntimeException ex){
			}		
		 }
    	
		return libraries;
    }
    
/*    public Library getLibrary(String collection) {
    	HashMap<String, Object> map = new HashMap<String,Object>();
	    map.put("uri", SharepointInitializer.getBaseUri() + '/' + "provider?collection=" + collection);
	    map.put("name", collection);
	    return new Library(map);
    }*/
 
    /*
     * get the documents 
     */
    public List<Map<String, ShareValue>> getDocuments() throws UnrecognizedValueTypeException {    	
    	HashMap<String, ShareValue> map;
    	
     	List<Map<String, ShareValue>> documents = new ArrayList<Map<String, ShareValue>>();
		for(String entitySet : odatac.getEntitySets()){
		   try {
			  // ignore ReportingTemplates collection, it returns documents we are not interested in
			  if (!entitySet.startsWith("ReportingTemplates")){
			     	
			     for(OEntity oe1 : odatac.getEntities(entitySet).filter("ContentType eq 'Document'").execute()) {
		        	  map = new HashMap<String,ShareValue>();
				      ShareValue shareValue = new ShareValue(ShareValueType.URI, URLStrategy.getResourceBaseURL() + '/' + entitySet + '/' + oe1.getProperty("Id").getValue());
				      map.put("uri", shareValue);		
				      shareValue = new ShareValue(ShareValueType.STRING, oe1.getProperty("Name").getValue());		
				      map.put("title", shareValue);
				      
				      documents.add(map);	
		         }
		          	      
			  }
			} catch(java.lang.RuntimeException ex){
			}    	
		 }	    	

		return documents;

	    }
    
    /*
     * retrieve the documents for a document library collection
     */
    public List<Map<String, ShareValue>> getDocuments(String collection) throws UnrecognizedValueTypeException { 	
    	HashMap<String, ShareValue> map; 
    	List<Map<String, ShareValue>> documents = new ArrayList<Map<String, ShareValue>>();

		try {
      	   //System.out.println(collection);
		  		      	
	       for(OEntity oe1 : odatac.getEntities(collection).filter("ContentType eq 'Document'").execute()) {
	      	  map = new HashMap<String,ShareValue>();
		      ShareValue shareValue = new ShareValue(ShareValueType.URI, URLStrategy.getResourceBaseURL() + '/' + collection + '/' + oe1.getProperty("Id").getValue());
		      map.put("uri", shareValue);		
		      shareValue = new ShareValue(ShareValueType.STRING, oe1.getProperty("Name").getValue());		
		      map.put("title", shareValue);
		      
		      documents.add(map);	
	       }  
		 } catch(java.lang.RuntimeException ex){
			 System.out.println("exception from odata");
		 }
	
		return documents;
    }
    
    protected String getLinkedEntity(OEntity entity, String link, String property) {
    	ORelatedEntityLink createdByLink =  entity.getLink(link, ORelatedEntityLink.class);
		OEntity ocb = odatac.getEntity(createdByLink).execute();
		return ocb.getProperty(property).getValue().toString();
    }
    
    /*
     * Retrieve the metadata about a documetn from sharepoint
     */
    public SharepointResource getDocumentProperties(String uri) throws UnrecognizedValueTypeException, ShareServerException {
		//System.out.println("getDocumentResource: uri='" + uri + "'");
		SharepointResource sharepointResource = new SharepointResource(uri);
    	//String uri = resource.getUri();
		try {
			//Split uri -- get /Empire for Entity, get /1 for id			
			//System.out.println("getDocumentProperties: entered; uri='" + uri +"'" );
			//System.out.println("getDocumentProperties: entered; " + URLStrategy.getResourceBaseURL().length()  );
			if (uri.startsWith(URLStrategy.getResourceBaseURL())) {
			   String temp = uri.substring(URLStrategy.getResourceBaseURL().length()+1);
			   String[] tokens = temp.split("/");

			   OEntity oe = (OEntity) odatac.getEntity(tokens[0], Integer.parseInt(tokens[1])).execute();
			   for (OProperty<?> p : oe.getProperties()) {
				   if (p.getName().equals("Id")) {
					   sharepointResource.setIdentifier(p.getValue().toString());
				   } else if (p.getName().equals("Name")) {
					   sharepointResource.setTitle((String) p.getValue());
				   } else if (p.getName().equals("CreatedById")) {
					   sharepointResource.setCreator(getLinkedEntity( oe, "CreatedBy", "Name"));
				   } else if (p.getName().equals("Created")) {
					//   System.out.println("Created: ='" + p.getValue().toString());
					   sharepointResource.setCreated(((LocalDateTime) p.getValue()).toDateTime().toDate());
				   } else if (p.getName().equals("Modified")) {
					 //  System.out.println("Modified: ='" + p.getValue().toString());
					   sharepointResource.setModified(((LocalDateTime) p.getValue()).toDateTime().toDate());
				   } else if (p.getName().equals("ModifiedById")) {
					   sharepointResource.setContributor(getLinkedEntity( oe, "ModifiedBy", "Name"));
				   } else if (p.getName().equals("ApprovalStatus")) {
					   sharepointResource.setApprovalStatus((String) p.getValue().toString());
				   }	
			
				}
			} else {// TODO problems
				System.out.println("uri did not start correctly");
			}
			
			
			
		} catch (Exception e) {
			System.err.println(e);
		}
		
 		
    	return sharepointResource;
    }
    
    /*
     * Create a document in sharepoint using the UI delegate
     */
    public  int createDocument(HttpServletResponse response, String library, FileItem item) throws UnrecognizedValueTypeException, ShareServerException {
	//	System.out.println("createDocument: uri='" + uri + "'");
		//SharepointResource sharepointResource = new SharepointResource(uri);
		
//    	String filename = resource.getSource();
//    	
//		OEntity newProduct = odatac.createEntity("Empire").properties(OProperties.int32("Id", 10))
//		                                                 .properties(OProperties.string("Name", filename))
//		                                                 .properties(OProperties.string("ContentType","Document"))
//		                                                 .properties(OProperties.string("Title","Architecture"))
//		                                                 .properties(OProperties.string("ApprovalStatus","2"))
//		                                                 .properties(OProperties.string("Path","/Empire"))	
//                                                         .execute();  
   
    	// no obvious API in odata4j to create a document, default apache http Create
	    HttpClient client = new HttpClient();
	    client.getParams().setParameter("http.useragent", "Test Client");

	    BufferedReader br = null;
	    int returnCode =500;
	    PostMethod method = null;
	    try {
    	  client.setConnectionTimeout(8000);   

	      method = new PostMethod(SharepointInitializer.getSharepointUri() + "/" + library); 
	      String userPassword = SharepointInitializer.getUsername() + ":" + SharepointInitializer.getPassword();
	    	 String encoding = Base64.encodeBase64String(userPassword.getBytes());
		  encoding = encoding.replaceAll("\r\n?", "");
    	  method.setRequestHeader("Authorization", "Basic " + encoding);
	      method.addRequestHeader("Content-type", item.getContentType());  
	      method.addRequestHeader(IConstants.HDR_SLUG, "/" + library + "/" + item.getName());
	      
	      //InputStream is =  new FileInputStream("E:\\odata\\sharepoint\\DeathStarTest.doc");
		          
          RequestEntity entity = new InputStreamRequestEntity(  
                        item.getInputStream(),  
                        "application/msword");  
          method.setRequestEntity(entity); 	
	      method.setDoAuthentication( true );	
	      
	      returnCode = client.executeMethod(method);

	      if(returnCode == HttpStatus.SC_NOT_IMPLEMENTED) {
	        System.err.println("The Post method is not implemented by this URI");
	        // still consume the response body
	        method.getResponseBodyAsString();
	      } else {
	        //br = new BufferedReader(new InputStreamReader(method.getResponseBodyAsStream()));
	    	InputStream is = method.getResponseBodyAsStream();
	    	//br = new BufferedReader(new InputStreamReader(is));	    	
		    //    String readLine;
		    //    while(((readLine = br.readLine()) != null)) {
		    //      System.out.println(readLine);
		    //    }
	    	
			response.setContentType("text/html");
			//response.setContentType("application/atom+xml");
			//response.setContentLength(is.getBytes().length);
			response.setStatus(IConstants.SC_OK);
			//response.getWriter().write("<html><head><title>hello world</title></head><body><p>hello world!</p></body></html>");
			//response.getWriter().write(method.getResponseBodyAsString());
			
			DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
			DocumentBuilder parser = factory.newDocumentBuilder();
			Document doc = parser.parse(is);       
			Element root = doc.getDocumentElement();

		    System.out.println ("Root element of the doc is " +   root.getNodeName());


			
//			String msftdPrefix = "http://schemas.microsoft.com/ado/2007/08/dataservices";
//			String msftmPrefix = "http://schemas.microsoft.com/ado/2007/08/dataservices/metadata";
			
			String id = null;
			String name = null;
			NodeList nl = root.getElementsByTagName("d:Id");			
			if (nl.getLength() > 0) {
               id = nl.item(0).getFirstChild().getNodeValue(); 
			}
			
			//nl = root.getElementsByTagName("d:ContentType");			
			//if (nl.getLength() > 0) {
            //   type = nl.item(0).getFirstChild().getNodeValue(); 
			//}
			
			nl = root.getElementsByTagName("d:Name");			
			if (nl.getLength() > 0) {
               name = nl.item(0).getFirstChild().getNodeValue(); 
			}
			 
		
			response.getWriter().write("<html>");
			response.getWriter().write("<head>");
			response.getWriter().write("</head>");
			response.getWriter().write("<body>");
			response.getWriter().write("<p>" + name + " was created with an Id =" + id +"</p>" );
			response.getWriter().write("</body>");
			response.getWriter().write("</html>");
			
	
			
			
			
			//response.getWriter().write(is.content);
			//String readLine;
		    //while(((readLine = br.readLine()) != null)) {
		    //	response.getWriter().write(readLine);
		    //}
	    	//response.setContentType(IConstants.CT_XML);
	        //response.getWriter().write(is.toString()); 
		    //response.setStatus(IConstants.SC_OK);
		    //test 
	     //   String readLine;
	     //   while(((readLine = br.readLine()) != null)) {
	     //     System.out.println(readLine);
	     //   }
	      }
	    } catch (Exception e) {
	      System.err.println(e);
	      e.printStackTrace();
	    } finally {
	      if (method != null) method.releaseConnection();
	      if(br != null) try { br.close(); } catch (Exception fe) {}
	    }
        return returnCode;
	  }
    
}
