import tempfile
from pathlib import Path
import textwrap
from lyo_model_sync.sync import sync_vocab

SAMPLE_TTL = textwrap.dedent('''
@prefix ex: <http://example.org/ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:MyClass a rdfs:Class ; rdfs:label "My Class" .
ex:myProperty a rdf:Property ; rdfs:label "myProperty" .
''')

SAMPLE_XML = '''<?xml version="1.0" encoding="UTF-8"?>
<oscl4j_vocabulary:Vocabularies xmlns:xmi="http://www.omg.org/XMI" xmlns:oscl4j_vocabulary="http://org.eclipse.lyo/oslc4j/vocabulary">
</oscl4j_vocabulary:Vocabularies>
'''


def test_sync_vocab_creates_entries(tmp_path):
    rdf_file = tmp_path / 'sample.ttl'
    rdf_file.write_text(SAMPLE_TTL)
    xml_file = tmp_path / 'vocab.xml'
    xml_file.write_text(SAMPLE_XML)

    # Run sync
    sync_vocab(str(xml_file), 'ex', str(rdf_file), 'http://example.org/ns#', dry_run=False)

    # Check result
    txt = xml_file.read_text()
    assert 'vocabularies' in txt
    assert 'MyClass' in txt
    assert 'myProperty' in txt

def test_sync_vocab_updates_existing(tmp_path):
    rdf_file = tmp_path / 'sample.ttl'
    rdf_file.write_text(SAMPLE_TTL)
    xml_file = tmp_path / 'vocab.xml'
    xml_file.write_text(SAMPLE_XML)

    # First run
    sync_vocab(str(xml_file), 'ex', str(rdf_file), 'http://example.org/ns#', dry_run=False)
    # Run again: should not error and should update attributes
    sync_vocab(str(xml_file), 'ex', str(rdf_file), 'http://example.org/ns#', dry_run=True)
