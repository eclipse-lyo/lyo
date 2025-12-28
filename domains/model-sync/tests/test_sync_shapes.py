import textwrap
from pathlib import Path
from lyo_model_sync.sync import sync_shapes

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
