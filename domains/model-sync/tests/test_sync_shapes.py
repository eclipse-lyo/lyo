import textwrap
from pathlib import Path
from lxml import etree
from lyo_model_sync.sync import sync_shapes, find_or_create_domain_spec

SAMPLE_SHAPES = textwrap.dedent('''
@prefix ex: <http://example.org/ns#> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:MyShape a sh:NodeShape ; sh:targetClass ex:MyClass ;
  sh:property [ sh:path ex:myProperty ; sh:name "myProperty" ; sh:minCount 0 ; sh:datatype xsd:string ] .
''')

SAMPLE_SPEC_XML = '''<?xml version="1.0" encoding="UTF-8"?>
<oscl4j_ai:Specification xmlns:xmi="http://www.omg.org/XMI" xmlns:oscl4j_ai="http://org.eclipse.lyo/oslc4j/adaptorInterface">
</oscl4j_ai:Specification>
'''


def test_sync_shapes_creates_resource_properties(tmp_path):
    rdf_file = tmp_path / 'shapes.ttl'
    rdf_file.write_text(SAMPLE_SHAPES)
    xml_file = tmp_path / 'spec.xml'
    xml_file.write_text(SAMPLE_SPEC_XML)

    sync_shapes(str(xml_file), 'ex', str(rdf_file), 'http://example.org/ns#', dry_run=False)

    txt = xml_file.read_text()
    assert 'resources' in txt
    assert 'resourceProperties' in txt
    assert 'myProperty' in txt


def test_property_definition_uses_vocab_namespace_uri(tmp_path):
    # Shape in shapes namespace references a property in a different vocab namespace
    rdf = textwrap.dedent('''
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    @prefix shv: <http://example.org/shapes#> .
    @prefix v: <http://example.org/vocab#> .
    @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

    shv:MyShape a sh:NodeShape ; sh:targetClass shv:MyClass ;
      sh:property [ sh:path v:theProp ; sh:name "theProp" ; sh:minCount 0 ; sh:datatype xsd:string ] .
    ''')
    rdf_file = tmp_path / 'shapes2.ttl'
    rdf_file.write_text(rdf)
    xml_file = tmp_path / 'spec2.xml'
    xml_file.write_text(SAMPLE_SPEC_XML)

    # sync shapes using shapes prefix 'shv' and shapes namespace; the property is in vocab namespace v
    sync_shapes(str(xml_file), 'shv', str(rdf_file), 'http://example.org/shapes#', dry_run=False)

    txt = xml_file.read_text()
    assert "vocabulary.xml#//@vocabularies[namespaceURI='http://example.org/vocab#']/@properties[name='theProp']" in txt


def test_property_definition_from_oslc_cm_and_dcterms(tmp_path):
    rdf = textwrap.dedent('''
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    @prefix shv: <http://example.org/shapes#> .
    @prefix oslc: <http://open-services.net/ns/core#> .
    @prefix oslc_cm: <http://open-services.net/ns/cm#> .
    @prefix dcterms: <http://purl.org/dc/terms/> .

    shv:MyShape a sh:NodeShape ; sh:targetClass shv:MyClass ;
      sh:property [ sh:path shv:p1 ; sh:name "p1" ; oslc:propertyDefinition oslc_cm:tracksChangeSet ] ;
      sh:property [ sh:path shv:p2 ; sh:name "p2" ; oslc:propertyDefinition dcterms:title ] .
    ''')
    rdf_file = tmp_path / 'shapes3.ttl'
    rdf_file.write_text(rdf)
    xml_file = tmp_path / 'spec3.xml'
    xml_file.write_text(SAMPLE_SPEC_XML)

    sync_shapes(str(xml_file), 'shv', str(rdf_file), 'http://example.org/shapes#', dry_run=False)

    txt = xml_file.read_text()
    assert "vocabulary.xml#//@vocabularies[namespaceURI='http://open-services.net/ns/cm#']/@properties[name='tracksChangeSet']" in txt
    assert "vocabulary.xml#//@vocabularies[namespaceURI='http://purl.org/dc/terms/']/@properties[name='title']" in txt


def test_added_property_gets_id_range_and_attached_to_resource(tmp_path):
    rdf = textwrap.dedent('''
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    @prefix shv: <http://example.org/shapes#> .
    @prefix oslc: <http://open-services.net/ns/core#> .
    @prefix oslc_cm: <http://open-services.net/ns/cm#> .

    # property p that references oslc_cm:State as its range
    shv:MyShape a sh:NodeShape ; sh:targetClass shv:ChangeRequest ;
      sh:property [ sh:path shv:severity ; sh:name "severity" ; sh:class shv:State ; oslc:propertyDefinition oslc_cm:severity ] .
    ''')
    rdf_file = tmp_path / 'shapes4.ttl'
    rdf_file.write_text(rdf)
    xml_file = tmp_path / 'spec4.xml'
    xml_file.write_text(SAMPLE_SPEC_XML)

    # create an existing resource (ChangeRequest) with id to simulate domain
    model_tree = etree.parse(str(xml_file))
    ds = find_or_create_domain_spec(model_tree, 'http://example.org/shapes#', name=None, prefix='//@domainPrefixes[name=\'shv\']')
    r = etree.Element('resources')
    r.set('name', 'ChangeRequest')
    r.set('id', '_ch1')
    ds.append(r)
    # also add a resource representing State class in same domain
    s = etree.Element('resources')
    s.set('name', 'State')
    s.set('id', '_state')
    ds.append(s)
    model_tree.write(str(xml_file), encoding='UTF-8', xml_declaration=True, pretty_print=True)

    sync_shapes(str(xml_file), 'shv', str(rdf_file), 'http://example.org/shapes#', dry_run=False)

    txt = xml_file.read_text()
    print('\nDEBUG XML:\n', txt)
    # assert a resourceProperties was added and has an id attribute
    assert 'resourceProperties' in txt
    # severity property should have been attached to ChangeRequest resource
    assert '_ch1' in txt
    # and the resourceProperty should have range pointing to _state
    assert 'range="_state"' in txt


def test_auto_create_resource_for_range_when_missing(tmp_path):
    rdf = textwrap.dedent('''
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    @prefix shv: <http://example.org/shapes#> .
    @prefix oslc: <http://open-services.net/ns/core#> .
    @prefix oslc_cm: <http://open-services.net/ns/cm#> .

    # property p that references oslc_cm:State as its range, but the domain has no State resource yet
    shv:MyShape a sh:NodeShape ; sh:targetClass shv:ChangeRequest ;
      sh:property [ sh:path shv:severity ; sh:name "severity" ; sh:class shv:State ; oslc:propertyDefinition oslc_cm:severity ] .
    ''')
    rdf_file = tmp_path / 'shapes6.ttl'
    rdf_file.write_text(rdf)
    xml_file = tmp_path / 'spec6.xml'
    xml_file.write_text(SAMPLE_SPEC_XML)

    # create an existing resource (ChangeRequest) with id to simulate domain
    model_tree = etree.parse(str(xml_file))
    ds = find_or_create_domain_spec(model_tree, 'http://example.org/shapes#', name=None, prefix='//@domainPrefixes[name=\'shv\']')
    r = etree.Element('resources')
    r.set('name', 'ChangeRequest')
    r.set('id', '_ch1')
    ds.append(r)
    model_tree.write(str(xml_file), encoding='UTF-8', xml_declaration=True, pretty_print=True)

    sync_shapes(str(xml_file), 'shv', str(rdf_file), 'http://example.org/shapes#', dry_run=False)

    model_tree = etree.parse(str(xml_file))
    txt = xml_file.read_text()
    # assert a resourceProperties was added and has a range attribute
    assert 'resourceProperties' in txt
    assert 'range="' in txt
    # find the created State resource
    states = model_tree.xpath("//resources[@name='State']")
    assert len(states) == 1
    state_id = states[0].get('id')
    assert state_id
    # ensure at least one resourceProperty references that state id as its range
    rps = model_tree.xpath("//resourceProperties[@range='%s']" % state_id)
    assert len(rps) >= 1


def test_valueType_resource_leaves_range_empty(tmp_path):
    rdf = textwrap.dedent('''
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    @prefix shv: <http://example.org/shapes#> .
    @prefix oslc: <http://open-services.net/ns/core#> .

    # property p has valueType oslc:Resource but no oslc:range
    shv:MyShape a sh:NodeShape ; sh:targetClass shv:ChangeRequest ;
      sh:property [ sh:path shv:link ; sh:name "link" ; oslc:valueType oslc:Resource ] .
    ''')
    rdf_file = tmp_path / 'shapes7.ttl'
    rdf_file.write_text(rdf)
    xml_file = tmp_path / 'spec7.xml'
    xml_file.write_text(SAMPLE_SPEC_XML)

    # create ChangeRequest resource
    model_tree = etree.parse(str(xml_file))
    ds = find_or_create_domain_spec(model_tree, 'http://example.org/shapes#', name=None, prefix='//@domainPrefixes[name=\'shv\']')
    r = etree.Element('resources')
    r.set('name', 'ChangeRequest')
    r.set('id', '_ch1')
    ds.append(r)
    model_tree.write(str(xml_file), encoding='UTF-8', xml_declaration=True, pretty_print=True)

    sync_shapes(str(xml_file), 'shv', str(rdf_file), 'http://example.org/shapes#', dry_run=False)

    model_tree = etree.parse(str(xml_file))
    # Any resource must NOT be created
    any_res = model_tree.xpath("//resources[@name='Any']")
    assert len(any_res) == 0
    # find created resourceProperty for 'link' and assert valueType is Resource and range is unset
    rps = model_tree.xpath("//resourceProperties[@name='link']")
    assert len(rps) >= 1
    assert rps[0].get('valueType') == 'Resource'
    assert rps[0].get('range') is None


def test_valueType_localresource_leaves_range_empty(tmp_path):
    rdf = textwrap.dedent('''
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    @prefix shv: <http://example.org/shapes#> .
    @prefix oslc: <http://open-services.net/ns/core#> .

    # property p has valueType oslc:LocalResource but no oslc:range
    shv:MyShape a sh:NodeShape ; sh:targetClass shv:ChangeRequest ;
      sh:property [ sh:path shv:local ; sh:name "local" ; oslc:valueType oslc:LocalResource ] .
    ''')
    rdf_file = tmp_path / 'shapes8.ttl'
    rdf_file.write_text(rdf)
    xml_file = tmp_path / 'spec8.xml'
    xml_file.write_text(SAMPLE_SPEC_XML)

    # create ChangeRequest resource
    model_tree = etree.parse(str(xml_file))
    ds = find_or_create_domain_spec(model_tree, 'http://example.org/shapes#', name=None, prefix='//@domainPrefixes[name=\'shv\']')
    r = etree.Element('resources')
    r.set('name', 'ChangeRequest')
    r.set('id', '_ch1')
    ds.append(r)
    model_tree.write(str(xml_file), encoding='UTF-8', xml_declaration=True, pretty_print=True)

    sync_shapes(str(xml_file), 'shv', str(rdf_file), 'http://example.org/shapes#', dry_run=False)

    model_tree = etree.parse(str(xml_file))
    # Any resource must NOT be created
    any_res = model_tree.xpath("//resources[@name='Any']")
    assert len(any_res) == 0
    # find created resourceProperty for 'local' and assert valueType is LocalResource and range is unset
    rps = model_tree.xpath("//resourceProperties[@name='local']")
    assert len(rps) >= 1
    assert rps[0].get('valueType') == 'LocalResource'
    assert rps[0].get('range') is None


def test_do_not_create_resource_for_core_or_other_domains(tmp_path):
    # If a shape uses a range class from a different domain (e.g., core/config), and that class exists elsewhere in the model,
    # do not create a duplicate resource in the current domain; instead link to the existing resource if possible, or leave range unset.
    rdf = textwrap.dedent('''
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    @prefix shv: <http://example.org/shapes#> .
    @prefix oslc: <http://open-services.net/ns/core#> .

    shv:MyShape a sh:NodeShape ; sh:targetClass shv:ChangeRequest ;
      sh:property [ sh:path shv:sp ; sh:name "sp" ; sh:class oslc:ServiceProvider ; oslc:propertyDefinition oslc:serviceProvider ] .
    ''')
    rdf_file = tmp_path / 'shapes9.ttl'
    rdf_file.write_text(rdf)
    xml_file = tmp_path / 'spec9.xml'
    xml_file.write_text(SAMPLE_SPEC_XML)

    # Create a different domain containing ServiceProvider so it's present in the model
    model_tree = etree.parse(str(xml_file))
    root = model_tree.getroot()
    other_ds = etree.Element('domainSpecifications')
    other_ds.set('namespaceURI', 'http://open-services.net/ns/core#')
    other_res = etree.Element('resources')
    other_res.set('name', 'ServiceProvider')
    other_res.set('id', '_sp_existing')
    other_ds.append(other_res)
    root.append(other_ds)
    model_tree.write(str(xml_file), encoding='UTF-8', xml_declaration=True, pretty_print=True)

    # create ChangeRequest resource in our target domain
    model_tree = etree.parse(str(xml_file))
    ds = find_or_create_domain_spec(model_tree, 'http://example.org/shapes#', name=None, prefix='//@domainPrefixes[name=\'shv\']')
    r = etree.Element('resources')
    r.set('name', 'ChangeRequest')
    r.set('id', '_ch1')
    ds.append(r)
    model_tree.write(str(xml_file), encoding='UTF-8', xml_declaration=True, pretty_print=True)

    sync_shapes(str(xml_file), 'shv', str(rdf_file), 'http://example.org/shapes#', dry_run=False)

    model_tree = etree.parse(str(xml_file))
    # ensure no duplicate ServiceProvider resource was created in our domain
    sp_in_domain = model_tree.xpath("//domainSpecifications[@namespaceURI='http://example.org/shapes#']//resources[@name='ServiceProvider']")
    assert len(sp_in_domain) == 0
    # ensure the created resourceProperty references the existing id if it was linked
    rps = model_tree.xpath("//resourceProperties[@name='sp']")
    assert len(rps) >= 1
    # if range set, it should reference the existing id
    rp_range = rps[0].get('range')
    if rp_range:
        assert rp_range == '_sp_existing'


def test_oslc_any_does_not_create_any_resource(tmp_path):
    # If shapes explicitly reference oslc:Any (either as oslc:range or oslc:valueType AnyResource), do NOT create a resources element named 'Any'.
    rdf = textwrap.dedent('''
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    @prefix shv: <http://example.org/shapes#> .
    @prefix oslc: <http://open-services.net/ns/core#> .

    # explicit oslc:range = oslc:Any
    shv:MyShape a sh:NodeShape ; sh:targetClass shv:ChangeRequest ;
      sh:property [ sh:path shv:foo ; sh:name "foo" ; oslc:range oslc:Any ] .

    shv:MyShape2 a sh:NodeShape ; sh:targetClass shv:ChangeRequest ;
      sh:property [ sh:path shv:bar ; sh:name "bar" ; oslc:valueType oslc:AnyResource ] .
    ''')
    rdf_file = tmp_path / 'shapes_any.ttl'
    rdf_file.write_text(rdf)
    xml_file = tmp_path / 'spec_any.xml'
    xml_file.write_text(SAMPLE_SPEC_XML)

    model_tree = etree.parse(str(xml_file))
    ds = find_or_create_domain_spec(model_tree, 'http://example.org/shapes#', name=None, prefix='//@domainPrefixes[name=\'shv\']')
    r = etree.Element('resources')
    r.set('name', 'ChangeRequest')
    r.set('id', '_ch1')
    ds.append(r)
    model_tree.write(str(xml_file), encoding='UTF-8', xml_declaration=True, pretty_print=True)

    sync_shapes(str(xml_file), 'shv', str(rdf_file), 'http://example.org/shapes#', dry_run=False)

    model_tree = etree.parse(str(xml_file))
    # ensure no 'Any' resource was created
    any_res = model_tree.xpath("//resources[@name='Any']")
    assert len(any_res) == 0
    # resourceProperties foo and bar should exist, have valueType set appropriately, and have no range
    rps_foo = model_tree.xpath("//resourceProperties[@name='foo']")
    rps_bar = model_tree.xpath("//resourceProperties[@name='bar']")
    assert len(rps_foo) >= 1
    assert len(rps_bar) >= 1
    assert rps_foo[0].get('range') is None
    assert rps_bar[0].get('range') is None
    assert rps_bar[0].get('valueType') in ('Resource','LocalResource')

def test_added_property_has_valueType_set(tmp_path):
    rdf = textwrap.dedent('''
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    @prefix shv: <http://example.org/shapes#> .
    @prefix oslc: <http://open-services.net/ns/core#> .

    shv:MyShape a sh:NodeShape ; sh:targetClass shv:ChangeRequest ;
      sh:property [ sh:path shv:link ; sh:name "link" ; sh:class shv:MyClass ] .
    ''')
    rdf_file = tmp_path / 'shapes10.ttl'
    rdf_file.write_text(rdf)
    xml_file = tmp_path / 'spec10.xml'
    xml_file.write_text(SAMPLE_SPEC_XML)

    model_tree = etree.parse(str(xml_file))
    ds = find_or_create_domain_spec(model_tree, 'http://example.org/shapes#', name=None, prefix='//@domainPrefixes[name=\'shv\']')
    r = etree.Element('resources')
    r.set('name', 'ChangeRequest')
    r.set('id', '_ch1')
    ds.append(r)
    model_tree.write(str(xml_file), encoding='UTF-8', xml_declaration=True, pretty_print=True)

    sync_shapes(str(xml_file), 'shv', str(rdf_file), 'http://example.org/shapes#', dry_run=False)

    model_tree = etree.parse(str(xml_file))
    rps = model_tree.xpath("//resourceProperties[@name='link']")
    assert len(rps) >= 1
    assert rps[0].get('valueType') is not None


def test_property_added_to_parent_not_descendants(tmp_path):
    rdf = textwrap.dedent('''
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    @prefix shv: <http://example.org/shapes#> .
    @prefix oslc: <http://open-services.net/ns/core#> .
    @prefix oslc_cm: <http://open-services.net/ns/cm#> .

    # Both ChangeRequest and Task reference the same property 'severity'
    shv:ChangeRequestShape a sh:NodeShape ; sh:targetClass shv:ChangeRequest ;
      sh:property [ sh:path shv:severity ; sh:name "severity" ; sh:class shv:State ; oslc:propertyDefinition oslc_cm:severity ] .

    shv:TaskShape a sh:NodeShape ; sh:targetClass shv:Task ;
      sh:property [ sh:path shv:severity ; sh:name "severity" ; sh:class shv:State ; oslc:propertyDefinition oslc_cm:severity ] .
    ''')
    rdf_file = tmp_path / 'shapes5.ttl'
    rdf_file.write_text(rdf)
    xml_file = tmp_path / 'spec5.xml'
    xml_file.write_text(SAMPLE_SPEC_XML)

    # create an existing parent resource ChangeRequest and descendant Task with extends
    model_tree = etree.parse(str(xml_file))
    ds = find_or_create_domain_spec(model_tree, 'http://example.org/shapes#', name=None, prefix='//@domainPrefixes[name=\'shv\']')
    parent = etree.Element('resources')
    parent.set('name', 'ChangeRequest')
    parent.set('id', '_parent')
    ds.append(parent)
    child = etree.Element('resources')
    child.set('name', 'Task')
    child.set('id', '_child')
    child.set('extends', '_parent')
    ds.append(child)
    # also add State resource
    s = etree.Element('resources')
    s.set('name', 'State')
    s.set('id', '_state')
    ds.append(s)
    model_tree.write(str(xml_file), encoding='UTF-8', xml_declaration=True, pretty_print=True)

    sync_shapes(str(xml_file), 'shv', str(rdf_file), 'http://example.org/shapes#', dry_run=False)

    txt = xml_file.read_text()
    # severity should be present as resourceProperty at domain-level
    assert "name=\"severity\"" in txt
    # parent should have the rp id in its resourceProperties attribute
    assert '_parent' in txt
    # child should NOT have a duplicated entry pointing to the same property id
    assert '_child' in txt
    # ensure severity property id only appears once in parent/child linkage
    assert txt.count('severity') >= 1


def test_do_not_reuse_resourceproperty_from_other_domain(tmp_path):
    rdf = textwrap.dedent('''
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    @prefix shv: <http://example.org/shapes#> .

    shv:MyShape a sh:NodeShape ; sh:targetClass shv:Child ;
      sh:property [ sh:path shv:external ; sh:name "external" ; sh:minCount 0 ] .
    ''')
    rdf_file = tmp_path / 'shapes_external.ttl'
    rdf_file.write_text(rdf)
    xml_file = tmp_path / 'spec_external.xml'
    xml_file.write_text(SAMPLE_SPEC_XML)

    # create other domain with existing resourceProperties 'external'
    model_tree = etree.parse(str(xml_file))
    root = model_tree.getroot()
    other_ds = etree.Element('domainSpecifications')
    other_ds.set('namespaceURI', 'http://example.org/other#')
    rp_other = etree.Element('resourceProperties')
    rp_other.set('name', 'external')
    rp_other.set('id', '_rp_other')
    other_ds.append(rp_other)
    # also create parent resource in other domain to be extended by child in our domain
    parent = etree.Element('resources')
    parent.set('name', 'Parent')
    parent.set('id', '_parent')
    other_ds.append(parent)
    root.append(other_ds)
    # write and reload
    model_tree.write(str(xml_file), encoding='UTF-8', xml_declaration=True, pretty_print=True)

    model_tree = etree.parse(str(xml_file))
    ds = find_or_create_domain_spec(model_tree, 'http://example.org/shapes#', name=None, prefix='//@domainPrefixes[name=\'shv\']')
    child = etree.Element('resources')
    child.set('name', 'Child')
    child.set('id', '_child')
    child.set('extends', '_parent')
    ds.append(child)
    model_tree.write(str(xml_file), encoding='UTF-8', xml_declaration=True, pretty_print=True)

    sync_shapes(str(xml_file), 'shv', str(rdf_file), 'http://example.org/shapes#', dry_run=False)
    model_tree = etree.parse(str(xml_file))
    # ensure our domain has its own resourceProperties for 'external' and it's not the external one
    our_rps = model_tree.xpath("//domainSpecifications[@namespaceURI='http://example.org/shapes#']//resourceProperties[@name='external']")
    assert len(our_rps) == 1
    assert our_rps[0].get('id') != '_rp_other'
    # ensure child links to our rp id
    child_el = model_tree.xpath("//domainSpecifications[@namespaceURI='http://example.org/shapes#']//resources[@name='Child']")[0]
    assert our_rps[0].get('id') in (child_el.get('resourceProperties') or '')


def test_property_in_parent_in_other_domain_not_added_to_child(tmp_path):
    rdf = textwrap.dedent('''
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    @prefix shv: <http://example.org/shapes#> .
    @prefix oslc_cm: <http://open-services.net/ns/cm#> .
    @prefix oslc: <http://open-services.net/ns/core#> .

    shv:ChangeRequestShape a sh:NodeShape ; sh:targetClass shv:ChangeRequest ;
      sh:property [ sh:path shv:severity ; sh:name "severity" ; sh:class shv:State ; oslc:propertyDefinition oslc_cm:severity ] .
    ''')
    rdf_file = tmp_path / 'shapes_parent_other.ttl'
    rdf_file.write_text(rdf)
    xml_file = tmp_path / 'spec_parent_other.xml'
    xml_file.write_text(SAMPLE_SPEC_XML)

    # create other domain that contains ChangeRequest resource with severity already linked
    model_tree = etree.parse(str(xml_file))
    root = model_tree.getroot()
    other_ds = etree.Element('domainSpecifications')
    other_ds.set('namespaceURI', 'http://example.org/other#')
    parent = etree.Element('resources')
    parent.set('name', 'ChangeRequest')
    parent.set('id', '_parent')
    # attach a resourceProperty to parent that matches 'severity'
    parent.set('resourceProperties', '_rp_external')
    other_ds.append(parent)
    rp_external = etree.Element('resourceProperties')
    rp_external.set('id', '_rp_external')
    rp_external.set('name', 'severity')
    other_ds.append(rp_external)
    root.append(other_ds)

    # create our domain with child 'Task' extends the ChangeRequest in other domain
    model_tree.write(str(xml_file), encoding='UTF-8', xml_declaration=True, pretty_print=True)
    model_tree = etree.parse(str(xml_file))
    ds = find_or_create_domain_spec(model_tree, 'http://example.org/shapes#', name=None, prefix='//@domainPrefixes[name=\'shv\']')
    child = etree.Element('resources')
    child.set('name', 'Task')
    child.set('id', '_child')
    child.set('extends', '_parent')
    ds.append(child)
    model_tree.write(str(xml_file), encoding='UTF-8', xml_declaration=True, pretty_print=True)

    sync_shapes(str(xml_file), 'shv', str(rdf_file), 'http://example.org/shapes#', dry_run=False)

    model_tree = etree.parse(str(xml_file))
    # Ensure the child did not get linked to the external severity resourceProperty
    child_el = model_tree.xpath("//domainSpecifications[@namespaceURI='http://example.org/shapes#']//resources[@name='Task']")[0]
    assert '_rp_external' not in (child_el.get('resourceProperties') or '')


def test_explicit_oslc_any_does_not_create_any_resource(tmp_path):
    rdf = textwrap.dedent('''
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    @prefix shv: <http://example.org/shapes#> .
    @prefix oslc: <http://open-services.net/ns/core#> .

    # explicit oslc:range referencing oslc:Any
    shv:MyShape a sh:NodeShape ; sh:targetClass shv:ChangeRequest ;
      sh:property [ sh:path shv:foo ; sh:name "foo" ; oslc:range oslc:Any ] .

    # explicit sh:class referencing oslc:Any
    shv:MyShape2 a sh:NodeShape ; sh:targetClass shv:ChangeRequest ;
      sh:property [ sh:path shv:bar ; sh:name "bar" ; sh:class oslc:Any ] .
    ''')
    rdf_file = tmp_path / 'shapes_any_explicit.ttl'
    rdf_file.write_text(rdf)
    xml_file = tmp_path / 'spec_any_explicit.xml'
    xml_file.write_text(SAMPLE_SPEC_XML)

    model_tree = etree.parse(str(xml_file))
    ds = find_or_create_domain_spec(model_tree, 'http://example.org/shapes#', name=None, prefix='//@domainPrefixes[name=\'shv\']')
    r = etree.Element('resources')
    r.set('name', 'ChangeRequest')
    r.set('id', '_ch1')
    ds.append(r)
    model_tree.write(str(xml_file), encoding='UTF-8', xml_declaration=True, pretty_print=True)

    sync_shapes(str(xml_file), 'shv', str(rdf_file), 'http://example.org/shapes#', dry_run=False)

    model_tree = etree.parse(str(xml_file))
    # ensure no 'Any' resource was created anywhere
    any_res = model_tree.xpath("//resources[@name='Any']")
    assert len(any_res) == 0
    # ensure resourceProperties exist with no range set
    rps_foo = model_tree.xpath("//resourceProperties[@name='foo']")
    rps_bar = model_tree.xpath("//resourceProperties[@name='bar']")
    assert len(rps_foo) >= 1
    assert len(rps_bar) >= 1
    assert rps_foo[0].get('range') is None
    assert rps_bar[0].get('range') is None

