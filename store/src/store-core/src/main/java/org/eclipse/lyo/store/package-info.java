/**
 * <p>Lyo Store is a library that can be used to persistently store OSLC resources in a
 * triplestore. This can be useful to cache the results of REST resource requests for a faster
 * retrieval of resources upon subsequent requests. Alternatively, a lifecycle tool may choose to
 * preload its OSLC resources in the triplestore, allowing its OSLC server to provide OSLC
 * services that interact directly with the artefacts consistently and conveniently managed using
 * the expected RDF technologies. This, for example, makes it relatively easier for an OSLC
 * server to provide clients with a TRS provider, or a SPARQL-endpoint for more advanced query
 * capabilities.</p>
 * <p>
 * <p> The Store expects the OSLC resources to be instances of
 * {@link org.eclipse.lyo.oslc4j.core.model.IResource} subclasses, with
 * appropriate OSLC annotations. Such subclasses can be defined manually. Better still, you can
 * use Lyo Toolchain to graphically model the domain and automatically generate OSLC4J-compliant
 * Java code.</p>
 * <p>
 * <p> The main entry point of this library is the {@link org.eclipse.lyo.store.Store}
 * interface. Three concrete store implementations are provided allowing for in-memory, on-disk
 * and a SPARQL-compatible Store implementation. You can instantiate any of these concrete
 * implementations using the {@link org.eclipse.lyo.store.StoreFactory} class.</p>
 *
 * @author Andrew Berezovskyi (andriib@kth.se)
 * @version $version-stub$
 * @since 0.14.0
 */
package org.eclipse.lyo.store;

/*-
 * #%L
 * Contributors:
 *  - Andrew Berezovskyi
 * %%
 * Copyright (C) 2016 KTH Royal Institute of Technology
 * %%
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * #L%
 */
