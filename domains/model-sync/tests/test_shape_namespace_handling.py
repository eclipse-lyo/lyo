from rdflib import Graph, URIRef, Literal, RDF, Namespace
from lyo_model_sync.sync import extract_shapes

OSLC = Namespace('http://open-services.net/ns/core#')
SHAPES_NS = 'https://www.omg.org/spec/SysML/20230201/shapes#'
VOCAB_NS = 'https://www.omg.org/spec/SysML/20230201/vocab#'


def make_graph_for_type_with_property():
    g = Graph()
    shape_uri = URIRef(SHAPES_NS + 'Type')
    prop_uri = URIRef(SHAPES_NS + 'typeOwnedDisjoining')
    tc = URIRef(VOCAB_NS + 'Type')
    # ResourceShape that describes vocab:Type and references the property
    g.add((shape_uri, RDF.type, OSLC.ResourceShape))
    g.add((shape_uri, OSLC.describes, tc))
    g.add((shape_uri, OSLC.property, prop_uri))
    # Property description
    g.add((prop_uri, RDF.type, OSLC.Property))
    g.add((prop_uri, OSLC.name, Literal('typeOwnedDisjoining')))
    g.add((prop_uri, OSLC.valueType, URIRef('http://open-services.net/ns/core#Resource')))
    return g


def test_extract_shapes_with_shapes_namespace():
    g = make_graph_for_type_with_property()
    shapes = extract_shapes(g, SHAPES_NS)
    assert 'Type' in shapes
    prop_names = {p.get('name') for p in shapes['Type']}
    assert 'typeOwnedDisjoining' in prop_names


def test_extract_shapes_with_vocab_namespace():
    g = make_graph_for_type_with_property()
    shapes = extract_shapes(g, VOCAB_NS)
    # When a *vocab* namespace is supplied to extract_shapes, it should NOT return shapes
    # because sync-shapes requires the shapes namespace (shapes are subjects in shapes NS).
    assert 'Type' not in shapes
