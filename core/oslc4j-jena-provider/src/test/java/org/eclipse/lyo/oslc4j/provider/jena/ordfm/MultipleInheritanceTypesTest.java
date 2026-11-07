/* SPDX-License-Identifier: EPL-2.0 OR BSD-3-Clause */
package org.eclipse.lyo.oslc4j.provider.jena.ordfm;

import static org.junit.jupiter.api.Assertions.*;
import java.util.Set;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.vocabulary.RDF;
import org.eclipse.lyo.oslc4j.core.annotation.*;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class MultipleInheritanceTypesTest {
    @OslcName("Primary") @OslcNamespace("urn:mi:") @OslcResourceShape
    public static class Primary {}
    @OslcName("Secondary") @OslcNamespace("urn:mi:") @OslcResourceShape
    public static class Secondary {}
    @OslcName("Secondary") @OslcNamespace("urn:mi:") @OslcResourceShape
    public interface SecondaryContract {}
    @OslcName("Composite") @OslcNamespace("urn:mi:") @OslcResourceShape
    public static class Composite extends Primary implements SecondaryContract {}
    @OslcName("Leaf") @OslcNamespace("urn:mi:") @OslcResourceShape
    public static class Leaf extends Composite {}

    @BeforeEach void register() {
        ResourcePackages.reset();
        ResourcePackages.TYPES_MAPPINGS.put("urn:mi:Primary", Set.of(Primary.class));
        ResourcePackages.TYPES_MAPPINGS.put("urn:mi:Secondary", Set.of(Secondary.class));
        ResourcePackages.TYPES_MAPPINGS.put("urn:mi:Composite", Set.of(Composite.class));
        ResourcePackages.TYPES_MAPPINGS.put("urn:mi:Leaf", Set.of(Leaf.class));
    }
    @AfterEach void reset() { ResourcePackages.reset(); }

    @Test void selectsLeafAcrossPrimaryAndSecondaryBranchesRegardlessOfTypeOrder() {
        for (var names : java.util.List.of(java.util.List.of("Secondary", "Leaf", "Primary", "Composite"),
                java.util.List.of("Leaf", "Composite", "Primary", "Secondary"))) {
            var model = ModelFactory.createDefaultModel();
            var resource = model.createResource("urn:mi:item");
            names.forEach(name -> resource.addProperty(RDF.type, model.createResource("urn:mi:" + name)));
            assertEquals(Leaf.class, ResourcePackages.getClassOf(resource).orElseThrow());
        }
    }

    @Test void unrelatedTypesRemainAmbiguous() {
        var model = ModelFactory.createDefaultModel();
        var resource = model.createResource("urn:mi:item");
        resource.addProperty(RDF.type, model.createResource("urn:mi:Primary"));
        resource.addProperty(RDF.type, model.createResource("urn:mi:Secondary"));
        assertThrows(IllegalStateException.class, () -> ResourcePackages.getClassOf(resource));
    }
}
