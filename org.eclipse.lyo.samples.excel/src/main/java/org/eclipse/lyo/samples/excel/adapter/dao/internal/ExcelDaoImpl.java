/*******************************************************************************
 * Copyright (c) 2011 IBM Corporation.
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
 *     Masaki Wakao 
 *     Yoshio Horiuchi 
 *     Kohji Ohsawa 
 *******************************************************************************/
package org.eclipse.lyo.samples.excel.adapter.dao.internal;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;

import org.apache.poi.hssf.usermodel.HSSFCell;
import org.apache.poi.hssf.usermodel.HSSFRow;
import org.apache.poi.hssf.usermodel.HSSFSheet;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.eclipse.lyo.samples.excel.adapter.dao.ExcelDao;
import org.eclipse.lyo.samples.excel.adapter.dao.PropertyMappingConfig;
import org.eclipse.lyo.samples.excel.adapter.dao.PropertyMappingInfo;
import org.eclipse.lyo.samples.excel.adapter.dao.ResourceFactory;
import org.eclipse.lyo.samples.excel.common.ICmConstants;

import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.ResIterator;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.sparql.vocabulary.FOAF;
import com.hp.hpl.jena.vocabulary.DC;

public class ExcelDaoImpl implements ExcelDao {
	//TODO 
	private static final String DEFAULT_SHEET_NAME="defects";
	
	private final static Property IDENFIFIER = com.hp.hpl.jena.rdf.model.ResourceFactory.createProperty("http://purl.org/dc/elements/1.1/", "identifier");
	private final static Property TITLE = com.hp.hpl.jena.rdf.model.ResourceFactory.createProperty("http://purl.org/dc/elements/1.1/", "title");
	private final static Property DESCRIPTION = com.hp.hpl.jena.rdf.model.ResourceFactory.createProperty("http://purl.org/dc/elements/1.1/", "description");
	private final static Property CREATED = com.hp.hpl.jena.rdf.model.ResourceFactory.createProperty("http://purl.org/dc/elements/1.1/", "created");
	private final static Property CONTRIBUTER = com.hp.hpl.jena.rdf.model.ResourceFactory.createProperty("http://purl.org/dc/elements/1.1/", "contributor");
	private final static Property STATUS = com.hp.hpl.jena.rdf.model.ResourceFactory.createProperty("http://open-services.net/ns/cm#", "status");
	private final static Property RELATED_CHANGEREQUEST = com.hp.hpl.jena.rdf.model.ResourceFactory.createProperty("http://open-services.net/ns/cm#", "relatedChangeRequest");
	private final static Property PERSON_NAME = com.hp.hpl.jena.rdf.model.ResourceFactory.createProperty("http://xmlns.com/foaf/0.1/", "name");
	
	
	private String relationshipUri = null;
	private ResourceFactory resourceFactory = null;
	private PropertyMappingConfig config = null;

	public void setRelationshipUri(String relationshipUri) {
		this.relationshipUri = relationshipUri;
	}
	public void setResourceFactory(ResourceFactory resourceFactory) {
		this.resourceFactory = resourceFactory;
	}
	public void setPropertyMappingConfig(PropertyMappingConfig config) {
		this.config = config;
	}
	public Model parseFile(String fileName) {
		if (relationshipUri == null || config == null) {
			return null;
		}
		List<PropertyMappingInfo> mappingInfo = config.listPropertyInfo(new File(fileName).getName());
		
		FileInputStream in = null;
		Workbook wb = null;
		
		try{
			in = new FileInputStream(fileName);
			wb = WorkbookFactory.create(in);
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			try {
				in.close();
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		
		Model model = ModelFactory.createDefaultModel();
		model.setNsPrefix("oslc_cm", ICmConstants.OSLC_CM_NAMESPACE);
		model.setNsPrefix("foaf", FOAF.NS);
		
		Sheet sheet = wb.getSheetAt(0);
		for (int j = sheet.getFirstRowNum(); j <= sheet.getLastRowNum(); j++) {
			Row row = sheet.getRow(j);
			if (row == null) {
				continue;
			}
			
			Resource changeRequest = null;
			for (int i = 0; i < mappingInfo.size(); i++) {
				PropertyMappingInfo info = mappingInfo.get(i);
				if (info.getResourceName().equalsIgnoreCase(ICmConstants.OSLC_CM_CHANGEREQUEST) && 
					info.getPropertyName().equalsIgnoreCase(DC.identifier.getURI())) {
					String value = getCellValue(row, info);
					if (value != null) {
						changeRequest = resourceFactory.createResource(model, info.getResourceName(), value, info.getPropertyName());
						break;
					}
				}
			}
			if (changeRequest == null) {
				continue;
			}
			for (int i = 0; i < mappingInfo.size(); i++) {
				PropertyMappingInfo info = mappingInfo.get(i);
				Property property = model.createProperty(info.getPropertyName());
				String value = getCellValue(row, info);
				if (value == null) {
					continue;
				}
				if (info.getPropertyType().equalsIgnoreCase("http://open-services.net/ns/core#Resource")) { // TODO add to cm constant def
					PropertyMappingInfo detailInfo = info.getDetailInfo();
					String resourceClass = detailInfo.getResourceName();
					Resource resource = resourceFactory.createResource(model, resourceClass, value, detailInfo.getPropertyName());
					changeRequest.addProperty(property, resource);
				} else {
					Literal literal = model.createLiteral(value);
					changeRequest.addLiteral(property, literal);
				}
			}
		}
		return model;
	}
	
	public void write(String fileName, Model model, boolean append) throws IOException {
		HSSFWorkbook workBook = new HSSFWorkbook();
		if(append){
			FileInputStream in = new FileInputStream(fileName);
			try {
				workBook = (HSSFWorkbook) WorkbookFactory.create(in);
				in.close();
			} catch (InvalidFormatException e) {
				e.printStackTrace();
			}
		}
		HSSFSheet sheet = null;
		if(append){
			sheet = workBook.getSheet(DEFAULT_SHEET_NAME);
		}else{
			sheet = workBook.createSheet(DEFAULT_SHEET_NAME);
		}
		
		int count = 0;
		if(append){
			count = getNewId(fileName);
		}
		
		ResIterator resIter = model.listSubjects();
		while(resIter.hasNext()){
			HSSFRow row = null;
			Resource resource = (Resource)resIter.nextResource();
			
			List<PropertyMappingInfo> mappingInfo = config.listPropertyInfo(null); //try default mapping
			for (int j = 0; j < mappingInfo.size(); j++) {
				PropertyMappingInfo info = mappingInfo.get(j);
				if (info.getResourceName().equalsIgnoreCase(ICmConstants.OSLC_CM_CHANGEREQUEST) && 
						info.getPropertyName().equalsIgnoreCase(DC.identifier.getURI())) {
					//identifier
					Statement idenfifier = resource.getProperty(IDENFIFIER);
					if (idenfifier != null) {
						if(row == null){
							row = sheet.createRow(count++);
						}
						HSSFCell cell = row.createCell(info.getIndex());
						cell.setCellValue(idenfifier.getInt());
					}
//				}else if (info.getPropertyType().equalsIgnoreCase("http://open-services.net/ns/core#Resource") && 
//						info.getPropertyName().equalsIgnoreCase("http://purl.org/dc/elements/1.1/contributor")) { // TODO add to cm constant def
//					//contributor
//					Statement contributor = resource.getProperty(CONTRIBUTER);
//					if (contributor != null) {
//						if(row == null){
//							row = sheet.createRow(count++);
//						}
//						 						
//						PropertyMappingInfo detailInfo = info.getDetailInfo();
//						if(detailInfo.getPropertyType().equalsIgnoreCase("http://xmlns.com/foaf/0.1/Person") && 
//								detailInfo.getPropertyName().equalsIgnoreCase("http://xmlns.com/foaf/0.1/name")) { // TODO add to cm constant def
//							
//							Statement personName = contributor.getProperty(PERSON_NAME);
//							//Resource person = contributor.getProperty(property);
//							HSSFCell cell = row.createCell(info.getIndex());
//							cell.setCellValue(personName.getString());
//						}
//					}
//				}else if (info.getPropertyType().equalsIgnoreCase("http://open-services.net/ns/core#Resource") && 
//						info.getPropertyName().equalsIgnoreCase("http://open-services.net/ns/cm#relatedChangeRequest")) { // TODO add to cm constant def
//					//relatedChangeRequest
//					Statement relatedChangeRequest = resource.getProperty(RELATED_CHANGEREQUEST);
//					if (relatedChangeRequest != null) {
//						if(row == null){
//							row = sheet.createRow(count++);
//						}
//						HSSFCell cell = row.createCell(info.getIndex());
//						cell.setCellValue(relatedChangeRequest.getInt());
//					}
				} else if (info.getPropertyName().equalsIgnoreCase("http://purl.org/dc/elements/1.1/title")) { // TODO add to cm constant def
					//title
					Statement title = resource.getProperty(TITLE);
					if (title != null) {
						if(row == null){
							row = sheet.createRow(count++);
						}
						HSSFCell cell = row.createCell(info.getIndex());
						cell.setCellValue(title.getString());
					}
				} else if (info.getPropertyName().equalsIgnoreCase("http://purl.org/dc/elements/1.1/description")) { // TODO add to cm constant def
					//description
					Statement description = resource.getProperty(DESCRIPTION);
					if (description != null) {
						if(row == null){
							row = sheet.createRow(count++);
						}
						HSSFCell cell = row.createCell(info.getIndex());
						cell.setCellValue(description.getString());
					}
				} else if (info.getPropertyName().equalsIgnoreCase("http://purl.org/dc/elements/1.1/created")) { // TODO add to cm constant def
					//created
					Statement created = resource.getProperty(CREATED);
					if (created != null) {
						if(row == null){
							row = sheet.createRow(count++);
						}
						HSSFCell cell = row.createCell(info.getIndex());
						cell.setCellValue(created.getString());
					}
				} else if (info.getPropertyName().equalsIgnoreCase("http://open-services.net/ns/cm#status")) { // TODO add to cm constant def
					//status
					Statement status = resource.getProperty(STATUS);
					if (status != null) {
						if(row == null){
							row = sheet.createRow(count++);
						}
						HSSFCell cell = row.createCell(info.getIndex());
						cell.setCellValue(status.getString());
					}
				}
			}
		}

		OutputStream out = new FileOutputStream(fileName);
        workBook.write(out);
        out.close();

	}
	
	private String getCellValue(Row row, PropertyMappingInfo info) {
		int index = info.getIndex();
		if (index != -1) {
			Cell cell = row.getCell(index);
			if (cell != null) {
				String value = null;
				try {
					String type = info.getPropertyType();
					if (type.equalsIgnoreCase(ICmConstants.XSD_DATATYPE_DATETIME)) {
						String formatString = info.getFormatString();
						if (formatString == null) {
							formatString = "yyyy-MM-dd'T'HH:mm:ssz";
						}
						Date date = cell.getDateCellValue();
						SimpleDateFormat f = new SimpleDateFormat(formatString);
						value = f.format(date);
					}
				} catch (Exception e) {
				}
				if (value == null) {
					int type = cell.getCellType();
					if (type == Cell.CELL_TYPE_STRING) {
						value = cell.getStringCellValue();
					} else if (type == Cell.CELL_TYPE_NUMERIC) {
						double d = cell.getNumericCellValue();
						if (d == Math.floor(d)) { // need to consider when d is negative
							value = "" + (int) d;
						} else {
							value = "" + cell.getNumericCellValue();
						}
					}
				}
				return value;
			}
		}
		return null;
	}
	
	@Override
	public int getNewId(String fileName) {
		HSSFWorkbook workBook = new HSSFWorkbook();
		try {
			FileInputStream in = new FileInputStream(fileName);
			workBook = (HSSFWorkbook) WorkbookFactory.create(in);
			in.close();
		} catch (IOException e) {
			e.printStackTrace();
		} catch (InvalidFormatException e) {
			e.printStackTrace();
		}
		
		HSSFSheet sheet = workBook.getSheet(DEFAULT_SHEET_NAME);
		
		return sheet.getLastRowNum() + 1;
	}
}
