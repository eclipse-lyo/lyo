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
 *******************************************************************************/
package org.eclipse.lyo.samples.excel.adapter;

import java.io.FileInputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;

public class MapperTable {
	
	Map<String, MapperEntry> entryMap = new HashMap<String, MapperEntry>();
	List<String> nameList = new ArrayList<String>();
	
	public void initialize(String fileName) {
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

		String lastName = null;
		Sheet sheet = wb.getSheetAt(0); //wb.getFirstVisibleTab() + 1);
		int start = 2; // skip row 0 and 1
		for (int j = start; j <= sheet.getLastRowNum(); j++) {
			Row row = sheet.getRow(j);
			if (row == null) {
				continue;
			}
			
			/* 0,    1,    2,    3   */
			/* name, type, line, uri */
			String name = getCellValue(row, 0);
			if (name != null) {
				MapperEntry entry = new MapperEntry(name);
				entry.setType(getCellValue(row, 1)); /* type */
				entry.setLine(getCellValue(row, 2)); /* line */
				entry.setUri(getCellValue(row, 3));  /* uri */
				entryMap.put(name, entry);
				nameList.add(name);
				lastName = name;
				System.out.println("entry " + j + " : name=" + entry.getName() + 
						", type=" + entry.getType() + ", line=" + entry.getLine() + 
						", uri=" + entry.getUri());
				continue;
			}
			
			if (lastName != null) {
				/* 4,         5,         6,        7        */
				/* prop:name, prop:type, prop:col, prop:ref */
				String propName = getCellValue(row, 4);
				if (propName != null) {
					MapperEntry entry = entryMap.get(lastName);
					MapperEntry.Property prop = entry.addProperty(propName);
					prop.setType(getCellValue(row, 5));	/* prop:type */
					prop.setColumn(getCellValue(row, 6));	/* prop:col */
					prop.setReference(getCellValue(row, 7));	/* prop:ref */
					System.out.println("property : name=" + prop.getName() +
							", type=" + prop.getType() + ", column=" + prop.getColumn() +
							", reference=" + prop.getReference());
				}
			}
		}
	}
	public List<String> getNameList() {
		return nameList;
	}
	public MapperEntry getEntry(String name) {
		return entryMap.get(name);
	}
	
	private String getCellValue(Row row, int index) {
		if (row != null) {
			return getCellValue(row.getCell(index));
		}
		return null;
	}
	private String getCellValue(Cell cell) {
		String value = null;
		if (cell != null) {
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
