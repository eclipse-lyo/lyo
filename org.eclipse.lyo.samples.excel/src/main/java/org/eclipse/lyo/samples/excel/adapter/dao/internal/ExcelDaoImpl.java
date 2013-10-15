/*******************************************************************************
 * Copyright (c) 2011,2013 IBM Corporation.
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

import java.io.FileInputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.time.FastDateFormat;
import org.apache.poi.hssf.usermodel.HSSFSheet;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CreationHelper;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.ss.usermodel.FormulaEvaluator;
import org.apache.poi.ss.usermodel.Name;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.apache.poi.ss.util.AreaReference;
import org.apache.poi.ss.util.CellReference;
import org.eclipse.lyo.samples.excel.adapter.MapperEntry;
import org.eclipse.lyo.samples.excel.adapter.MapperTable;
import org.eclipse.lyo.samples.excel.adapter.dao.ExcelDao;
import org.eclipse.lyo.samples.excel.common.ConfigSingleton;

import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.vocabulary.RDF;

public class ExcelDaoImpl implements ExcelDao {
	//TODO 
	private static final String DEFAULT_SHEET_NAME="defects";
	
	private final static String DEFAULT_OUTPUT_DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SSSZZ";
	
	private String relationshipUri = null;
	private MapperTable mapperTable = null;

	public void setRelationshipUri(String relationshipUri) {
		this.relationshipUri = relationshipUri;
	}
	public void setMapperTable(MapperTable mapperTable) {
		this.mapperTable = mapperTable;
	}
	
	public Model parseFile(String fileName) {
		if (relationshipUri == null) {
			return null;
		}

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
		model.setNsPrefixes(ConfigSingleton.getInstance().getNsPrefixes());

		HashMap<Sheet, Object[]> sheetResourceMap = new HashMap<Sheet, Object[]>();

		// Loop for Resources defined in Mapper file
		for (String en : mapperTable.getNameList()) {
			MapperEntry e = mapperTable.getEntry(en);
			String type = e.getType();
			String line = e.getLine();
			String uri = e.getUri();
			
			// parse line definition in Mapper file
			String[] ls = line.split(",");
			if(ls.length < 3){
				System.err.println("line must has at least sheet, start row, and end row information");
				continue;
			}
			String ssheet = ls[0].trim();
			String sstart = ls[1].trim();
			String send = ls[2].trim();
			Sheet sheet = null;
			try{
				sheet = wb.getSheetAt(Integer.parseInt(ssheet));
			}catch(NumberFormatException ex){
				sheet = wb.getSheet(ssheet);
			}
			if(sheet==null){
				System.err.println("target sheet is not found");
				continue;
			}
			
			int start = Integer.parseInt(sstart);
			int end = sheet.getLastRowNum();
			if (!send.equals("*")) {
				end = Integer.parseInt(send);
			}
			String cond_cellstring = null;
			boolean exist = true;
			if(ls.length > 3){
				String scond = ls[3].trim();
				if (scond.startsWith("exist")) {
					cond_cellstring = scond.substring(6, scond.length() - 1).trim();
				}else if(scond.startsWith("notexist")) {
					exist = false;					
					cond_cellstring = scond.substring(9, scond.length() - 1).trim();
				}
			}
			
			// map to find referenced resource later
			Object[] resourceMap = sheetResourceMap.get(sheet);
			if(resourceMap == null){
				resourceMap = new Object[sheet.getLastRowNum() + 1];
				Arrays.fill(resourceMap, null);
				sheetResourceMap.put(sheet, resourceMap);
			}

			// Loop of excel table rows to find the resource 
			for (int j = start; j <= end; j++) {
				if (sheet.getRow(j) == null) {
					continue;
				}
				if (cond_cellstring != null) {
					Cell cell = getCell(sheet, cond_cellstring, j);
					String value = getCellValue(cell);
					if (value == null && exist || value != null && !exist){
						continue;
					} 
				}
				// generate URI for this resource
				String[] uris = uri.split(",");
				String format = uris[0].trim();
				String uriString = format;
				if (uris.length == 3) {
					Cell cell = getCell(sheet, uris[1].trim(), j);
					String value1 = getCellValue(cell);
					cell = getCell(sheet, uris[2].trim(), j);
					String value2 = getCellValue(cell);
					uriString = String.format(format, value1, value2);
				} else if (uris.length == 2) {
					Cell cell = getCell(sheet, uris[1].trim(), j);
					String value = getCellValue(cell);
					uriString = String.format(format, value);
				}
				
				// create a Resource in RDF model with URI and resource type defined in Mapper file
				Resource resource = null;
				try {
					resource = model.createResource(relationshipUri + URLEncoder.encode(uriString, "UTF-8"));

					type = getNameUri(type.trim(), model);
					resource.addProperty(RDF.type, model.createResource(type));
				} catch (UnsupportedEncodingException e1) {
					e1.printStackTrace();
				}
				if (resource == null) {
					continue;
				}
				
				// Keep resource map for current row which will be used to generate reference URI later
				Map<String, Resource> curResMap = (Map<String, Resource>) resourceMap[j];
				if (curResMap == null) {
					curResMap = new HashMap<String, Resource>();
					resourceMap[j] = curResMap;
				}
				curResMap.put(en, resource);
				
				// Loop for Properties for this resource defined in Mapper file
				for (String propName: e.getPropertyNameList()) {
					MapperEntry.Property prop = e.getProperty(propName);
					if (prop == null) {
						continue;
					}
					String propType = prop.getType();
					if (propType == null) {
						continue;
					}
					if (propType.equalsIgnoreCase("resource")) {
						// assume that prop contains "reference" information in Mapper file
						String reference = prop.getReference();
						if (reference != null) {
							processReference(model, resource, propName,	reference, resourceMap, j);
						}
					} else {
						// assume that prop contains "column" information in Mapper file
						String[] tokens = prop.getColumn().trim().split(",");
						String fmt = null;
						String column = tokens[0];
						if (tokens.length > 1) {
							fmt = tokens[0];
							column = tokens[1];
						}
						Cell cell = getCell(sheet, column, j);
						if (cell != null) {
							String value = getCellValue(cell);
							if (value != null) {
								if (fmt != null) {
									value = String.format(fmt, value);
								}
								String qpname = propName.trim();
								qpname = getNameUri(qpname, model);
								Property property = model.createProperty(qpname);
								Literal literal = model.createLiteral(value);
								resource.addLiteral(property, literal);
							}
						}
					}
				}
			}
		}
		return model;
	}
	
	private Cell getCell(Sheet sheet, String cellRowString, int defaultRowIndex) {
		Cell cell = getNamedCell(sheet, cellRowString, defaultRowIndex);
		if (cell != null) {
			return cell;
		}
		int[] index = cellRowStringToIndex(cellRowString);
		int rowIndex = (index.length > 1) ? index[1] : defaultRowIndex;
		Row row = sheet.getRow(rowIndex);
		if (row != null) {
			return row.getCell(index[0]);
		}
		return null;
	}
	
	private Cell getNamedCell(Sheet sheet, String cellRowString, int defaultRowIndex) {
		Name name = sheet.getWorkbook().getName(cellRowString);
		if (name != null) {
			AreaReference areaRef = new AreaReference(name.getRefersToFormula());
			CellReference firstCell = areaRef.getFirstCell();
			CellReference lastCell = areaRef.getLastCell();
			int rowIndex = defaultRowIndex;
			if (rowIndex < firstCell.getRow() || lastCell.getRow() < rowIndex) {
				rowIndex = firstCell.getRow();
			}
			Row row = sheet.getRow(rowIndex);
			if (row != null) {
				return row.getCell(firstCell.getCol());
			}
		}
		return null;
	}
	
	private int[] cellRowStringToIndex(String cellRowString) {
		int index = -1;
		for (int i = 0; i < cellRowString.length(); i++) {
			char c = cellRowString.charAt(i);
			if (Character.isDigit(c)) {
				 index = i;
				 break;
			}
		}
		if (index <= 0) {
			// No digit, or digit only.
			// The string only has a cell index in this case.
			return new int[] { cellStringToIndex(cellRowString) };
		}
		// The string has both cell and row indices.
		String cellString = cellRowString.substring(0, index);
		String rowString = cellRowString.substring(index);
		return new int[] { cellStringToIndex(cellString), Integer.parseInt(rowString) }; 
	}
	
	private int cellStringToIndex(String cellString){
		try {
			return Integer.parseInt(cellString);
		}catch(NumberFormatException ex){
			return CellReference.convertColStringToIndex(cellString);
		}
	}
	
	private void processReference(Model model, Resource resource, String propName, String referenceDef, Object[] resourceMaps, int currentRow) {
		referenceDef = referenceDef.trim();
		int suffixIndex;

		// sameLine
		suffixIndex = referenceDef.toLowerCase().indexOf("[sameline]");
		if (suffixIndex > 0) {
			String targetResourceName = referenceDef.substring(0, suffixIndex);
			Map<String, Resource> resMap = (Map<String, Resource>) resourceMaps[currentRow];
			addReferenceProperty(model, resource, resMap, propName, targetResourceName);
		}

		// mostRecent
		suffixIndex = referenceDef.toLowerCase().indexOf("[mostrecent]");
		if (suffixIndex > 0)  {
			String targetResourceName = referenceDef.substring(0, suffixIndex);
			for (int i = currentRow; i >= 0; i--) { // mostrecent includes sameline
				Map<String, Resource> resMap = (Map<String, Resource>) resourceMaps[i];
				if(addReferenceProperty(model, resource, resMap, propName, targetResourceName))
					break;
			}
		}
	}

	private boolean addReferenceProperty(Model model, Resource resource, Map<String, Resource> resourceMap, String propName, String target) {
		if(resourceMap != null){
			Resource targetResource = resourceMap.get(target);
			if (targetResource != null) {
				String qpname = propName.trim();
				qpname = getNameUri(qpname, model);
				Property property = model.createProperty(qpname);
				resource.addProperty(property, targetResource);

				String backLinkUri = ConfigSingleton.getInstance().getBacklinks().get(qpname);
				if(backLinkUri != null){
					Property backLinkProperty = model.createProperty(backLinkUri);
					targetResource.addProperty(backLinkProperty, resource);					
				}
				return true;
			}
		}
		return false;
	}
	
	private String getCellValue(Cell cell) {
		if (cell != null) {
			String value = null;
			int type = cell.getCellType();
			if (type == Cell.CELL_TYPE_STRING) {
				value = cell.getStringCellValue();
			} else if (type == Cell.CELL_TYPE_NUMERIC) {
				if (DateUtil.isCellDateFormatted(cell)) {
					Date date = cell.getDateCellValue();
					value = FastDateFormat.getInstance(DEFAULT_OUTPUT_DATE_FORMAT).format(date);
				} else {
					double d = cell.getNumericCellValue();
					if (d == Math.floor(d)) { // need to consider when d is negative
						value = "" + (int) d;
					} else {
						value = "" + cell.getNumericCellValue();
					}
				}
			} else if (type == Cell.CELL_TYPE_FORMULA){
				// get calculated value if the cell type is formula 
				Workbook wb = cell.getSheet().getWorkbook();
				CreationHelper crateHelper = wb.getCreationHelper();
				FormulaEvaluator evaluator = crateHelper.createFormulaEvaluator();
				// get recursively if the value is still formula 
				value = getCellValue(evaluator.evaluateInCell(cell));
			}
			return value;
		}
		return null;
	}
	private String getNameUri(String name, Model model) {
		Map<String, String> prefixMapping = model.getNsPrefixMap();
		Set<String> keys = prefixMapping.keySet();
		Iterator<String> ite = keys.iterator();
		String prefix = null;
		while (ite.hasNext()) {
			String key = ite.next();
			if (name.startsWith(key + ":")) {
				prefix = key;
			}
		}
		if (prefix != null) {
			String uri = model.getNsPrefixURI(prefix);
			return name.replaceFirst(prefix + ":", uri);
		}
		if (name.startsWith("dcterms:")) {
			return name.replaceFirst("dcterms:", "http://purl.org/dc/terms/");
		}
		if (name.startsWith("dc:")) {
			return name.replaceFirst("dc:", "http://purl.org/dc/terms/");
		}
		return name;
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
