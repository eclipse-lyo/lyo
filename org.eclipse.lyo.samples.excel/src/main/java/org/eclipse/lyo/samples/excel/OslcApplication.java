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
package org.eclipse.lyo.samples.excel;

import java.util.HashSet;
import java.util.Set;

import javax.ws.rs.core.Application;

import org.eclipse.lyo.samples.excel.changerequest.ChangeRequestCompactService;
import org.eclipse.lyo.samples.excel.changerequest.ChangeRequestCreatorService;
import org.eclipse.lyo.samples.excel.changerequest.ChangeRequestGeneratorService;
import org.eclipse.lyo.samples.excel.changerequest.ChangeRequestListService;
import org.eclipse.lyo.samples.excel.changerequest.ChangeRequestReader;
import org.eclipse.lyo.samples.excel.services.common.CatalogService;
import org.eclipse.lyo.samples.excel.services.common.CreationFactoryService;
import org.eclipse.lyo.samples.excel.services.common.QueryService;
import org.eclipse.lyo.samples.excel.services.common.ResourceService;
import org.eclipse.lyo.samples.excel.services.common.ServiceProviderService;
import org.eclipse.lyo.samples.excel.services.common.SparqlQueryService;


public class OslcApplication extends Application {

    @Override
    public Set<Class<?>> getClasses() {
        Set<Class<?>> serviceClasses = new HashSet<Class<?>>();
        serviceClasses.add(ServiceProviderService.class);
        serviceClasses.add(CatalogService.class);
        serviceClasses.add(QueryService.class);
        serviceClasses.add(ResourceService.class);
        serviceClasses.add(CreationFactoryService.class);
        serviceClasses.add(ChangeRequestReader.class);
        serviceClasses.add(ChangeRequestGeneratorService.class);
        serviceClasses.add(SparqlQueryService.class);
        serviceClasses.add(ChangeRequestListService.class);
        serviceClasses.add(ChangeRequestCompactService.class);
        serviceClasses.add(ChangeRequestCreatorService.class);
        
        return serviceClasses;
    }
}
