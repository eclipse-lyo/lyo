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

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Random;
import java.util.Set;
import javax.xml.datatype.DatatypeConfigurationException;
import org.apache.jena.rdf.model.Model;
import org.assertj.core.api.Assertions;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.IResource;
import org.eclipse.lyo.oslc4j.core.model.ServiceProviderCatalog;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.eclipse.lyo.store.resources.BlankResource;
import org.eclipse.lyo.store.resources.Requirement;
import org.eclipse.lyo.store.resources.WithBlankResource;
import org.eclipse.lyo.store.resources.WithTwoDepthBlankResource;
import org.junit.Ignore;
import org.junit.Test;
import static org.assertj.core.api.Assertions.*;


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
    public void testStoreHasNoMissingKey() {
        // ARRANGE
        final T manager = buildStore();

        // ACT
        final boolean isNonExistentKeyCached = manager.namedGraphExists(URI.create("testKey"));

        // ASSERT
        Assertions.assertThat(isNonExistentKeyCached).isFalse();
    }

    @Test
    public void testStoreHasAddedKey() throws StoreAccessException {
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
            throws StoreAccessException, ModelUnmarshallingException {
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
            throws StoreAccessException {
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
            throws StoreAccessException, ModelUnmarshallingException {
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
            throws StoreAccessException, ModelUnmarshallingException {
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
            throws StoreAccessException, ModelUnmarshallingException {
        final T manager = buildStore();
        final URI testKeyAdd = buildKey();
        final IResource resource = buildResource();
        final IResource resource2 = buildResource();
        final IResource resource3 = buildResource();
        final ArrayList<IResource> resources = new ArrayList<>();
        resources.add(resource);
        resources.add(resource2);
        resources.add(resource3);

        manager.appendResources(testKeyAdd, resources);

        ServiceProviderCatalog resourceUnderKey = manager.getResource(testKeyAdd,
                resource2.getAbout(), ServiceProviderCatalog.class);

        Assertions.assertThat(resourceUnderKey).isNotNull();
        Assertions.assertThat(resourceUnderKey.getAbout().equals(resource2.getAbout()));
    }

    @Test(expected = NoSuchElementException.class)
    public void testMissingResourceException()
            throws StoreAccessException, ModelUnmarshallingException, URISyntaxException {
        final T manager = buildStore();
        final URI testKeyAdd = buildKey();
        final IResource resource = buildResource();
        final IResource resource2 = buildResource();
        final IResource resource3 = buildResource();
        final ArrayList<IResource> resources = new ArrayList<>();
        resources.add(resource);
        resources.add(resource2);
        resources.add(resource3);

        manager.appendResources(testKeyAdd, resources);

        manager.getResource(testKeyAdd, new URI("urn:blabla"), ServiceProviderCatalog.class);
    }


    @Test
    public void testStoreKeySetReturnsCorrectKeys()
            throws StoreAccessException {
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

    @Test
    public void testBlankNodeRetrieval()
            throws URISyntaxException, StoreAccessException,
            ModelUnmarshallingException {

        BlankResource aBlankResource = new BlankResource();
        aBlankResource.setIntProperty(1);
        final URI blankResourceURI = new URI("urn:1");
        WithBlankResource r1WithBlankResource = new WithBlankResource(blankResourceURI);
        r1WithBlankResource.setRelatesToBlankResource(aBlankResource);
        r1WithBlankResource.setStringProperty("some String");

        final T manager = buildStore();

        final URI namedGraphUri = new URI("urn:test");
        manager.putResources(namedGraphUri, ImmutableList.of(r1WithBlankResource));

        final WithBlankResource resource = manager.getResource(
                namedGraphUri,
                blankResourceURI,
                WithBlankResource.class);

        assertThat(resource.getRelatesToBlankResource().getIntProperty()).isEqualTo(1);
    }

    @Test
    public void testBlankNodeRetrievalDouble()
            throws URISyntaxException, StoreAccessException,
            ModelUnmarshallingException {

        BlankResource anotherBlankResource = new BlankResource();
        final int intProperty = 1;
        anotherBlankResource.setIntProperty(intProperty);
        WithBlankResource r2WithBlankResource = new WithBlankResource();
        r2WithBlankResource.setRelatesToBlankResource(anotherBlankResource);
        final String some_string = "some String";
        r2WithBlankResource.setStringProperty(some_string);
        final URI blankResourceURI = new URI("urn:2");
        WithTwoDepthBlankResource aWithTwoDepthBlankResource = new WithTwoDepthBlankResource(
                blankResourceURI);
        aWithTwoDepthBlankResource.setIntProperty(1);
        aWithTwoDepthBlankResource.setRelatesToBlankResourceTwoDepth(r2WithBlankResource);

        final T manager = buildStore();

        final URI namedGraphUri = new URI("urn:test");
        manager.putResources(namedGraphUri, ImmutableList.of(aWithTwoDepthBlankResource));

        final WithTwoDepthBlankResource resource = manager.getResource(namedGraphUri,
                                                                       blankResourceURI,
                                                                       WithTwoDepthBlankResource
                                                                               .class);

        assertThat(resource.getRelatesToBlankResourceTwoDepth().getStringProperty()).isEqualTo(
                some_string);
        assertThat(resource.getRelatesToBlankResourceTwoDepth()
                           .getRelatesToBlankResource()
                           .getIntProperty()).isEqualTo(intProperty);
    }

    @Test
    public void testStoreQueryForAllRequirementResources()
            throws StoreAccessException, ModelUnmarshallingException, URISyntaxException {
        final T manager = buildStore();
        final URI namedGraphUri = buildKey();
        populateStore(manager, namedGraphUri);

        List<Requirement> requirements = manager.getResources(namedGraphUri, Requirement.class, null, null, "", -1, -1);
        Assertions.assertThat(requirements).hasSize(6);
    }

    @Test
    public void testStoreQueryForRequirementResourcesWithFreeTextSearch()
            throws StoreAccessException, ModelUnmarshallingException, URISyntaxException {
        final T manager = buildStore();
        final URI namedGraphUri = buildKey();
        populateStore(manager, namedGraphUri);

        List<Requirement> requirements = manager.getResources(namedGraphUri, Requirement.class, null, null, "river", -1, -1);
        Assertions.assertThat(requirements).hasSize(2);
    }

    @Test
    public void testStoreQueryForRequirementResourcesWithWhereFilter()
            throws StoreAccessException, ModelUnmarshallingException, URISyntaxException {
        final T manager = buildStore();
        final URI namedGraphUri = buildKey();
        populateStore(manager, namedGraphUri);

        List<Requirement> requirements = manager.getResources(namedGraphUri, Requirement.class,
                "dcterms=<http://purl.org/dc/terms/>", "dcterms:identifier=\"observations\"", null, -1, -1);
        Assertions.assertThat(requirements).hasSize(1);
    }

    @Test
    public void testStoreQueryForRequirementResourcesWithFreeTextSearchAndWhereFilter()
            throws StoreAccessException, ModelUnmarshallingException, URISyntaxException {
        final T manager = buildStore();
        final URI namedGraphUri = buildKey();
        populateStore(manager, namedGraphUri);

        List<Requirement> requirements = manager.getResources(namedGraphUri, Requirement.class,
                "dcterms=<http://purl.org/dc/terms/>", "dcterms:identifier=\"observations\"", "roof", -1, -1);
        Assertions.assertThat(requirements).hasSize(1);
    }

    @Test
    public void testStoreQueryForRequirementResourcesWithNoMatch()
            throws StoreAccessException, ModelUnmarshallingException, URISyntaxException {
        final T manager = buildStore();
        final URI namedGraphUri = buildKey();
        populateStore(manager, namedGraphUri);

        List<Requirement> requirements = manager.getResources(namedGraphUri, Requirement.class,
                "dcterms=<http://purl.org/dc/terms/>", "dcterms:identifier=\"observations\"", "velocity", -1, -1);
        Assertions.assertThat(requirements).hasSize(0);
    }

    @Test
    public void testStoreQueryForAllResources()
            throws StoreAccessException, ModelUnmarshallingException, URISyntaxException {
        final T manager = buildStore();
        final URI namedGraphUri = buildKey();
        populateStore(manager, namedGraphUri);

        Model model = manager.getResources(namedGraphUri, null, null, "", -1, -1);
        Assertions.assertThat(model.listSubjects().toList()).hasSize(7);
    }

    @Test
    public void testStoreQueryForAllResourcesWithFreeTextSearch()
            throws StoreAccessException, ModelUnmarshallingException, URISyntaxException {
        final T manager = buildStore();
        final URI namedGraphUri = buildKey();
        populateStore(manager, namedGraphUri);

        Model model = manager.getResources(namedGraphUri, null, null, "river", -1, -1);
        Assertions.assertThat(model.listSubjects().toList()).hasSize(2);

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
    
	private Requirement createRequirement(String identifier, String description) throws URISyntaxException {
		Requirement r = new Requirement(buildKey());
		r.setIdentifier(identifier);
		r.setDescription(description);
		return r;
	}

    private void populateStore(final T manager, final URI namedGraphUri)
            throws StoreAccessException, URISyntaxException {
        manager.appendResource(namedGraphUri,
                createRequirement("rob", "Tom got a small piece of pie. Rock music approaches at high velocity."));
        manager.appendResource(namedGraphUri, createRequirement("hang",
                "She borrowed the book from him many years ago and hasn't yet returned it. Please wait outside of the house. The river stole the gods."));
        manager.appendResource(namedGraphUri, createRequirement("observations",
                "We have never been to Asia, nor have we visited Africa. Malls are great places to shop; I can find everything I need under one roof."));
        manager.appendResource(namedGraphUri, createRequirement("kindly",
                "I think I will buy the red car, or I will lease the blue one. The river stole the gods."));
        manager.appendResource(namedGraphUri, createRequirement("itch",
                "A song can make or ruin a person’s day if they let it get to them. The body may perhaps compensates for the loss of a true metaphysics."));
        manager.appendResource(namedGraphUri, createRequirement("morning",
                "They got there early, and they got really good seats. Lets all be unique together until we realise we are all the same."));
        manager.appendResource(namedGraphUri, buildResource());
    }

}

