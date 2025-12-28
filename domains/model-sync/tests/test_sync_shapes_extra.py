import textwrap
from lxml import etree
from lyo_model_sync.sync import sync_shapes, find_or_create_domain_spec

SAMPLE_SPEC_XML = '''<?xml version="1.0" encoding="UTF-8"?>
<oscl4j_ai:Specification xmlns:xmi="http://www.omg.org/XMI" xmlns:oscl4j_ai="http://org.eclipse.lyo/oslc4j/adaptorInterface">
</oscl4j_ai:Specification>
'''


def test_existing_resourceproperty_keeps_valueType(tmp_path):
    rdf = textwrap.dedent('''
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    @prefix shv: <http://example.org/shapes#> .

    shv:MyShape a sh:NodeShape ; sh:targetClass shv:ChangeRequest ;
      sh:property [ sh:path shv:description ; sh:name "description" ] .
    ''')
    rdf_file = tmp_path / 'shapes_desc.ttl'
    rdf_file.write_text(rdf)
    xml_file = tmp_path / 'spec_desc.xml'
    xml_file.write_text(SAMPLE_SPEC_XML)

    model_tree = etree.parse(str(xml_file))
    ds = find_or_create_domain_spec(model_tree, 'http://example.org/shapes#', name=None, prefix='//@domainPrefixes[name=\'shv\']')
    # pre-create a resourceProperty with valueType=String
    rp = etree.Element('resourceProperties')
    rp.set('id', '_rp_pre')
    rp.set('name', 'description')
    rp.set('valueType', 'String')
    ds.append(rp)
    model_tree.write(str(xml_file), encoding='UTF-8', xml_declaration=True, pretty_print=True)

    sync_shapes(str(xml_file), 'shv', str(rdf_file), 'http://example.org/shapes#', dry_run=False)

    model_tree = etree.parse(str(xml_file))
    rps = model_tree.xpath("//resourceProperties[@name='description']")
    assert len(rps) >= 1
    # ensure at least one preserved valueType attribute
    assert any(r.get('valueType') == 'String' for r in rps)


def test_new_property_defaults_to_literal(tmp_path):
    rdf = textwrap.dedent('''
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    @prefix shv: <http://example.org/shapes#> .

    shv:MyShape a sh:NodeShape ; sh:targetClass shv:ChangeRequest ;
      sh:property [ sh:path shv:foo ; sh:name "foo" ] .
    ''')
    rdf_file = tmp_path / 'shapes_foo.ttl'
    rdf_file.write_text(rdf)
    xml_file = tmp_path / 'spec_foo.xml'
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
    rps = model_tree.xpath("//resourceProperties[@name='foo']")
    assert len(rps) >= 1
    assert rps[0].get('valueType') == 'Literal'
