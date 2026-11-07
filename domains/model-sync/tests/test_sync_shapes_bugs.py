import textwrap
from pathlib import Path
from lxml import etree
from lyo_model_sync.sync import sync_shapes, find_or_create_domain_spec

SAMPLE_SPEC_XML = '''<?xml version="1.0" encoding="UTF-8"?>
<oscl4j_ai:Specification xmlns:xmi="http://www.omg.org/XMI" xmlns:oscl4j_ai="http://org.eclipse.lyo/oslc4j/adaptorInterface">
</oscl4j_ai:Specification>
'''


def test_auto_created_resource_has_describes_element(tmp_path):
    # Bug: When auto-creating a resources element for a missing range class,
    # we must include a <describes href="..."/> pointing to the vocabulary
    rdf = textwrap.dedent('''
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    @prefix shv: <http://example.org/shapes#> .
    @prefix oslc: <http://open-services.net/ns/core#> .
    @prefix oslc_cm: <http://open-services.net/ns/cm#> .

    # severity property references oslc_cm:Severity which doesn't exist as a resource yet
    shv:MyShape a sh:NodeShape ; sh:targetClass shv:ChangeRequest ;
      sh:property [ sh:path shv:severity ; sh:name "severity" ; oslc:propertyDefinition oslc_cm:severity ; oslc:range oslc_cm:Severity ] .
    ''')
    rdf_file = tmp_path / 'shapes_severity.ttl'
    rdf_file.write_text(rdf)
    xml_file = tmp_path / 'spec_severity.xml'
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
    # Severity resource should be created
    severity_res = model_tree.xpath("//resources[@name='Severity']")
    assert len(severity_res) == 1
    # It MUST have a describes element with href pointing to the vocabulary
    describes = severity_res[0].findall('.//describes')
    assert len(describes) == 1
    href = describes[0].get('href')
    assert href is not None
    # href should reference vocabulary.xml and the oslc_cm namespace
    assert 'vocabulary.xml' in href
    assert 'http://open-services.net/ns/cm#' in href
    assert 'Severity' in href


def test_existing_property_in_other_domain_not_duplicated(tmp_path):
    # Bug: created property from dcterms already exists in core domain;
    # when CM shapes references it, we should NOT create a new resourceProperty in CM domain
    rdf = textwrap.dedent('''
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    @prefix shv: <http://example.org/shapes#> .
    @prefix oslc: <http://open-services.net/ns/core#> .
    @prefix dcterms: <http://purl.org/dc/terms/> .
    @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

    shv:MyShape a sh:NodeShape ; sh:targetClass shv:ChangeRequest ;
      sh:property [ sh:path shv:created ; sh:name "created" ; oslc:propertyDefinition dcterms:created ; oslc:valueType xsd:dateTime ] .
    ''')
    rdf_file = tmp_path / 'shapes_created.ttl'
    rdf_file.write_text(rdf)
    xml_file = tmp_path / 'spec_created.xml'
    xml_file.write_text(SAMPLE_SPEC_XML)

    # Create core domain with existing 'created' resourceProperty
    model_tree = etree.parse(str(xml_file))
    root = model_tree.getroot()
    core_ds = etree.Element('domainSpecifications')
    core_ds.set('namespaceURI', 'http://purl.org/dc/terms/')
    core_rp = etree.Element('resourceProperties')
    core_rp.set('name', 'created')
    core_rp.set('id', '_rp_created_core')
    core_rp.set('valueType', 'DateTime')
    core_ds.append(core_rp)
    root.append(core_ds)

    # Create our target domain with ChangeRequest
    ds = find_or_create_domain_spec(model_tree, 'http://example.org/shapes#', name=None, prefix='//@domainPrefixes[name=\'shv\']')
    r = etree.Element('resources')
    r.set('name', 'ChangeRequest')
    r.set('id', '_ch1')
    ds.append(r)
    model_tree.write(str(xml_file), encoding='UTF-8', xml_declaration=True, pretty_print=True)

    sync_shapes(str(xml_file), 'shv', str(rdf_file), 'http://example.org/shapes#', dry_run=False)

    model_tree = etree.parse(str(xml_file))
    # Ensure NO new 'created' resourceProperty was created in our shapes domain
    our_rps = model_tree.xpath("//domainSpecifications[@namespaceURI='http://example.org/shapes#']//resourceProperties[@name='created']")
    assert len(our_rps) == 0
    # Ensure ChangeRequest references the existing core created property
    cr_el = model_tree.xpath("//resources[@name='ChangeRequest']")[0]
    cr_rps = cr_el.get('resourceProperties') or ''
    assert '_rp_created_core' in cr_rps
    # Ensure the existing core created property still has valueType=DateTime
    core_created = model_tree.xpath("//resourceProperties[@id='_rp_created_core']")[0]
    assert core_created.get('valueType') == 'DateTime'
