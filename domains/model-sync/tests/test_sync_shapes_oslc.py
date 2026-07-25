import textwrap
from pathlib import Path
from lyo_model_sync.sync import sync_shapes

SAMPLE_OSLC = textwrap.dedent('''
@prefix ex: <http://example.org/ns#> .
@prefix oslc: <http://open-services.net/ns/core#> .
@prefix dcterms: <http://purl.org/dc/terms/> .

ex:MyShape a oslc:ResourceShape ; oslc:describes ex:MyClass ; oslc:property <http://example.org/ns#myProp> .

<http://example.org/ns#myProp> a oslc:Property ; oslc:name "myProp" ; oslc:valueType oslc:Resource ; oslc:occurs oslc:Zero-or-many .
''')

SAMPLE_SPEC_XML = '''<?xml version="1.0" encoding="UTF-8"?>
<oscl4j_ai:Specification xmlns:xmi="http://www.omg.org/XMI" xmlns:oscl4j_ai="http://org.eclipse.lyo/oslc4j/adaptorInterface">
</oscl4j_ai:Specification>
'''


def test_sync_shapes_oslc_creates_resource_properties(tmp_path):
    rdf_file = tmp_path / 'shapes.ttl'
    rdf_file.write_text(SAMPLE_OSLC)
    xml_file = tmp_path / 'spec.xml'
    xml_file.write_text(SAMPLE_SPEC_XML)

    sync_shapes(str(xml_file), 'ex', str(rdf_file), 'http://example.org/ns#', dry_run=False)

    txt = xml_file.read_text()
    assert 'resourceProperties' in txt
    assert 'myProp' in txt
