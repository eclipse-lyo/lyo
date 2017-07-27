package org.eclipse.lyo.store;

/*-
 * #%L
 * Contributors:
 *      Andrew Berezovskyi - initial implementation
 * %%
 * Copyright (C) 2016 - 2017 KTH Royal Institute of Technology
 * %%
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * #L%
 */

import com.google.common.collect.Lists;

import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Random;
import java.util.Set;
import org.assertj.core.api.Assertions;
import org.eclipse.lyo.oslc4j.core.model.IResource;
import org.eclipse.lyo.oslc4j.core.model.ServiceProviderCatalog;
import org.junit.Test;

/**
 * StoreTestBase is .
 *
 * @author Andrew Berezovskyi (andriib@kth.se)
 * @version $version-stub$
 * @since 0.15.2
 */
public abstract class StoreTestBase<T extends Store> {

    public static final Random RANDOM = new Random(System.currentTimeMillis());

    @Test
    public void testStoreHasNoMissingKey() throws IOException {
        // ARRANGE
        final T manager = buildStore();

        // ACT
        final boolean isNonExistentKeyCached = manager.namedGraphExists(URI.create("testKey"));

        // ASSERT
        Assertions.assertThat(isNonExistentKeyCached).isFalse();
    }

    @Test
    public void testStoreHasAddedKey() throws IOException, StoreAccessException {
        final T manager = buildStore();
        final URI testKeyAdd = buildKey();
        final IResource resource = buildResource();

        manager.putResources(testKeyAdd, Collections.singletonList(resource));

        final boolean isAddedKeyCached = manager.namedGraphExists(testKeyAdd);
        Assertions.assertThat(isAddedKeyCached)
                .as("check if namedGraph '%s' was added", testKeyAdd)
                .isTrue();
    }

    @Test
    public void testStoreReadsSameValue()
            throws IOException, StoreAccessException, ModelUnmarshallingException {
        final T manager = buildStore();
        final URI testKeyAdd = buildKey();
        final IResource resource = buildResource();
        final URI testResourceURI = resource.getAbout();

        manager.putResources(testKeyAdd, Collections.singletonList(resource));
        final Collection<ServiceProviderCatalog> catalogs = manager.getResources(testKeyAdd,
                ServiceProviderCatalog.class);

        Assertions.assertThat(catalogs).hasSize(1);
        final ServiceProviderCatalog catalog = catalogs.toArray(new ServiceProviderCatalog[0])[0];
        Assertions.assertThat(catalog).isNotNull();
        Assertions.assertThat(catalog.getAbout().toASCIIString())
                .isEqualTo(testResourceURI.toASCIIString());
    }

    @Test
    public void testStorePurgedKeyRemoved()
            throws IOException, StoreAccessException, ModelUnmarshallingException {
        final T manager = buildStore();
        final URI testKeyAdd = buildKey();
        final IResource resource = buildResource();

        manager.putResources(testKeyAdd, Collections.singletonList(resource));
        manager.clear(testKeyAdd);

        final boolean isPurgedKeyInStore = manager.namedGraphExists(testKeyAdd);
        Assertions.assertThat(isPurgedKeyInStore).isFalse();
    }

    @Test
    public void testStoreSuccessivePutOverwrites()
            throws IOException, StoreAccessException, ModelUnmarshallingException {
        final T manager = buildStore();
        final URI testKeyAdd = buildKey();
        final IResource resource = buildResource();
        final IResource resource2 = buildResource();

        manager.putResources(testKeyAdd, Collections.singletonList(resource));
        manager.putResources(testKeyAdd, Collections.singletonList(resource2));

        final Collection<ServiceProviderCatalog> catalogs = manager.getResources(testKeyAdd,
                ServiceProviderCatalog.class);

        Assertions.assertThat(catalogs).hasSize(1);
        ServiceProviderCatalog[] catalogsArray = catalogs.toArray(new ServiceProviderCatalog[0]);
        Assertions.assertThat(catalogsArray[0]).isNotNull();
        Assertions.assertThat(catalogsArray[0].getAbout()).isEqualTo(resource2.getAbout());
    }

    @Test
    public void testStoreSuccessiveAddCombines()
            throws IOException, StoreAccessException, ModelUnmarshallingException {
        final T manager = buildStore();
        final URI testKeyAdd = buildKey();
        final IResource resource = buildResource();
        final IResource resource2 = buildResource();

        manager.appendResource(testKeyAdd, resource);
        manager.appendResource(testKeyAdd, resource2);

        final Collection<ServiceProviderCatalog> catalogs = manager.getResources(testKeyAdd,
                ServiceProviderCatalog.class);

        Assertions.assertThat(catalogs).hasSize(2);
        Assertions.assertThat(Lists.newArrayList(catalogs)
                .stream()
                .map((serviceProviderCatalog) -> serviceProviderCatalog.getAbout().toASCIIString()))
                .contains(resource.getAbout().toASCIIString(),
                        resource2.getAbout().toASCIIString());
    }

    @Test
    public void testStorePagingWorks()
            throws IOException, StoreAccessException, ModelUnmarshallingException {
        final T manager = buildStore();
        final URI testKeyAdd = buildKey();
        ArrayList<IResource> resources = new ArrayList<>();
        for (int i = 0; i < 100; i++) {
            IResource resource = buildResource();
            resources.add(resource);
            manager.appendResource(testKeyAdd, resource);
        }

        final Collection<ServiceProviderCatalog> catalogs = manager.getResources(testKeyAdd,
                ServiceProviderCatalog.class, 51, 0);
        final Collection<ServiceProviderCatalog> catalogs2 = manager.getResources(testKeyAdd,
                ServiceProviderCatalog.class, 51, 50);

        Assertions.assertThat(catalogs).hasSize(51);
        Assertions.assertThat(catalogs2).hasSize(50);
    }

    @Test
    public void testSingleResourceRetrieved()
            throws IOException, StoreAccessException, ModelUnmarshallingException {
        final T manager = buildStore();
        final URI testKeyAdd = buildKey();
        final IResource resource = buildResource();
        final IResource resource2 = buildResource();
        final IResource resource3 = buildResource();
        final ArrayList<IResource> resources = new ArrayList<IResource>();
        resources.add(resource);
        resources.add(resource2);
        resources.add(resource3);

        manager.appendResources(testKeyAdd, resources);

        ServiceProviderCatalog resourceUnderKey = manager.getResource(testKeyAdd,
                resource2.getAbout(), ServiceProviderCatalog.class);

        Assertions.assertThat(resourceUnderKey).isNotNull();
        Assertions.assertThat(resourceUnderKey.getAbout().equals(resource2.getAbout()));
    }

    @Test
    public void testStoreKeySetReturnsCorrectKeys()
            throws IOException, StoreAccessException, ModelUnmarshallingException {
        final T manager = buildStore();
        final URI key1 = buildKey();
        final URI key2 = buildKey();
        Assertions.assertThat(key1).isNotEqualTo(key2);
        final IResource resource = buildResource();

        manager.appendResource(key1, resource);
        manager.appendResource(key2, resource);
        Set<String> keySet = manager.keySet();

        Assertions.assertThat(keySet).hasSize(2);
        Assertions.assertThat(keySet).contains(key1.toString(), key2.toString());
    }

    protected abstract T buildStore();

    private URI buildKey() {
        return URI.create("lyo:testKey_" + randomHexString());
    }

    private static String randomHexString() {
        return Long.toHexString(RANDOM.nextLong());
    }

    private IResource buildResource() {
        final ServiceProviderCatalog resource = new ServiceProviderCatalog();
        resource.setAbout(URI.create("lyo:spc_" + randomHexString()));
        return resource;
    }
}

