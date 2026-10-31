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


def test_term_status_overrides_comment_for_property(tmp_path):
    rdf = textwrap.dedent('''
    @prefix ex: <http://example.org/ns#> .
    @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
    @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix vs: <http://www.w3.org/2003/06/sw-vocab-status/ns#> .

    ex:myProperty a rdf:Property ;
      rdfs:label "myProperty" ;
      rdfs:comment "original comment" ;
      vs:term_status "archaic" .
    ''')
    rdf_file = tmp_path / 'sample2.ttl'
    rdf_file.write_text(rdf)
    xml_file = tmp_path / 'vocab2.xml'
    xml_file.write_text(SAMPLE_XML)

    sync_vocab(str(xml_file), 'ex', str(rdf_file), 'http://example.org/ns#', dry_run=False)

    txt = xml_file.read_text()
    # comment should be replaced by the term_status value
    assert 'comment="archaic"' in txt


def test_term_status_overrides_comment_for_class(tmp_path):
    rdf = textwrap.dedent('''
    @prefix ex: <http://example.org/ns#> .
    @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
    @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix vs: <http://www.w3.org/2003/06/sw-vocab-status/ns#> .

    ex:MyClass a rdfs:Class ;
      rdfs:label "My Class" ;
      rdfs:comment "original comment" ;
      vs:term_status "archaic" .
    ''')
    rdf_file = tmp_path / 'sample3.ttl'
    rdf_file.write_text(rdf)
    xml_file = tmp_path / 'vocab3.xml'
    xml_file.write_text(SAMPLE_XML)

    sync_vocab(str(xml_file), 'ex', str(rdf_file), 'http://example.org/ns#', dry_run=False)

    txt = xml_file.read_text()
    assert 'MyClass' in txt
    assert 'comment="archaic"' in txt


def test_non_archaic_term_status_does_not_override_property_comment(tmp_path):
    rdf = textwrap.dedent('''
    @prefix ex: <http://example.org/ns#> .
    @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
    @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix vs: <http://www.w3.org/2003/06/sw-vocab-status/ns#> .

    ex:myProperty a rdf:Property ;
      rdfs:label "myProperty" ;
      rdfs:comment "original comment" ;
      vs:term_status "deprecated" .
    ''')
    rdf_file = tmp_path / 'sample4.ttl'
    rdf_file.write_text(rdf)
    xml_file = tmp_path / 'vocab4.xml'
    xml_file.write_text(SAMPLE_XML)

    sync_vocab(str(xml_file), 'ex', str(rdf_file), 'http://example.org/ns#', dry_run=False)

    txt = xml_file.read_text()
    # comment should remain the original comment
    assert 'comment="original comment"' in txt


def test_non_archaic_term_status_does_not_override_class_comment(tmp_path):
    rdf = textwrap.dedent('''
    @prefix ex: <http://example.org/ns#> .
    @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
    @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix vs: <http://www.w3.org/2003/06/sw-vocab-status/ns#> .

    ex:MyClass a rdfs:Class ;
      rdfs:label "My Class" ;
      rdfs:comment "original comment" ;
      vs:term_status "deprecated" .
    ''')
    rdf_file = tmp_path / 'sample5.ttl'
    rdf_file.write_text(rdf)
    xml_file = tmp_path / 'vocab5.xml'
    xml_file.write_text(SAMPLE_XML)

    sync_vocab(str(xml_file), 'ex', str(rdf_file), 'http://example.org/ns#', dry_run=False)

    txt = xml_file.read_text()
    assert 'MyClass' in txt
    assert 'comment="original comment"' in txt
