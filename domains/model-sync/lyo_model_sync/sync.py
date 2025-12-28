import logging
from rdflib import Graph, RDF, RDFS, OWL, URIRef, BNode, Namespace
from rdflib.namespace import SH
from lxml import etree
from pathlib import Path

log = logging.getLogger('lyo_model_sync')

def parse_rdf(rdf_path):
    g = Graph()
    try:
        # rdflib sometimes treats Windows paths as URLs; convert to file:// URI when path exists
        p = Path(rdf_path)
        if p.exists():
            g.parse(p.as_uri())
        else:
            g.parse(rdf_path)
        log.info('Parsed RDF: %s (triples=%d)', rdf_path, len(g))
    except Exception as e:
        log.error('Failed to parse RDF %s: %s', rdf_path, e)
        raise
    return g

def local_name(uri):
    if isinstance(uri, URIRef):
        s = str(uri)
        if '#' in s:
            return s.split('#')[-1]
        else:
            return s.rstrip('/').split('/')[-1]
    return None

def extract_vocab_entities(g, namespace_uri):
    ns = str(namespace_uri)
    classes = {}
    properties = {}
    ns_prefix = None

    for s in set(g.subjects()):
        if isinstance(s, URIRef) and str(s).startswith(ns):
            # detect class
            types = set(g.objects(s, RDF.type))
            lname = local_name(s)
            if any(t in (RDFS.Class, OWL.Class) for t in types):
                label = first_literal(g, s, RDFS.label)
                comment = first_literal(g, s, RDFS.comment)
                classes[lname] = {'uri': str(s), 'label': label, 'comment': comment}
            # detect property
            if any(t in (RDF.Property, OWL.ObjectProperty, OWL.DatatypeProperty) for t in types):
                label = first_literal(g, s, RDFS.label)
                comment = first_literal(g, s, RDFS.comment)
                properties[lname] = {'uri': str(s), 'label': label, 'comment': comment}
    # Fallback: look for properties with predicate in the namespace
    for p in set(g.predicates()):
        if isinstance(p, URIRef) and str(p).startswith(ns):
            lname = local_name(p)
            if lname not in properties:
                properties[lname] = {'uri': str(p), 'label': None, 'comment': None}
    return classes, properties


def first_literal(g, s, p):
    for o in g.objects(s, p):
        if o and (o.language is None or isinstance(o, str) or o):
            return str(o)
    return None


def find_or_create_vocab_element(model_tree, namespace_uri, label=None, prefix=None):
    root = model_tree.getroot()
    # look for <vocabularies namespaceURI="...">
    for vocab_el in root.findall('.//{*}vocabularies'):
        if vocab_el.get('namespaceURI') == namespace_uri:
            return vocab_el
    # not found -> create
    vocab_el = etree.Element('vocabularies')
    vocab_el.set('namespaceURI', namespace_uri)
    if label:
        vocab_el.set('label', label)
    if prefix:
        vocab_el.set('preferredNamespacePrefix', prefix)
    root.append(vocab_el)
    return vocab_el


def sync_vocab(model_path, prefix, rdf_path, namespace_uri, dry_run=False):
    g = parse_rdf(rdf_path)
    classes, properties = extract_vocab_entities(g, namespace_uri)

    model_tree = etree.parse(model_path)
    vocab_el = find_or_create_vocab_element(model_tree, namespace_uri, label=None, prefix=prefix)

    existing_classes = {el.get('name'): el for el in vocab_el.findall('.//{*}classes')}
    existing_properties = {el.get('name'): el for el in vocab_el.findall('.//{*}properties')}

    # Add/update classes
    for name, meta in classes.items():
        if name in existing_classes:
            el = existing_classes[name]
            if meta.get('label'):
                el.set('label', meta['label'])
            if meta.get('comment'):
                el.set('comment', meta['comment'])
            log.info('Updated class: %s', name)
        else:
            new = etree.Element('classes')
            new.set('name', name)
            if meta.get('label'):
                new.set('label', meta['label'])
            if meta.get('comment'):
                new.set('comment', meta['comment'])
            vocab_el.append(new)
            log.info('Added class: %s', name)

    # Add/update properties
    for name, meta in properties.items():
        if name in existing_properties:
            el = existing_properties[name]
            if meta.get('label'):
                el.set('label', meta['label'])
            if meta.get('comment'):
                el.set('comment', meta['comment'])
            log.info('Updated property: %s', name)
        else:
            new = etree.Element('properties')
            new.set('name', name)
            if meta.get('label'):
                new.set('label', meta['label'])
            if meta.get('comment'):
                new.set('comment', meta['comment'])
            vocab_el.append(new)
            log.info('Added property: %s', name)

    # Warn about extraneous entities in model not present in RDF
    for name in existing_classes.keys():
        if name not in classes:
            log.warning('Extraneous class in model (not in RDF): %s', name)
    for name in existing_properties.keys():
        if name not in properties:
            log.warning('Extraneous property in model (not in RDF): %s', name)

    if dry_run:
        log.info('Dry run mode; not saving changes to %s', model_path)
        return

    # write back in-place preserving original file's XML declaration
    model_tree.write(model_path, encoding='UTF-8', xml_declaration=True, pretty_print=True)
    log.info('Saved updated model to %s', model_path)

# --- Shapes handling ---

OSLC = Namespace('http://open-services.net/ns/core#')


def extract_shapes(g, namespace_uri):
    # Return: mapping class_localname -> list(properties)
    shapes = {}
    ns = str(namespace_uri)
    # SHACL NodeShapes
    for s in set(g.subjects(RDF.type, SH.NodeShape)):
        # For sync-shapes, the namespace argument refers to the *shapes* namespace;
        # require that the NodeShape subject is in that namespace.
        if isinstance(s, URIRef) and str(s).startswith(ns):
            for tc in g.objects(s, SH.targetClass):
                if isinstance(tc, URIRef):
                    cname = local_name(tc)
                    shapes[cname] = extract_shape_properties(g, s)
    # OSLC ResourceShapes
    for s in set(g.subjects(OSLC.describes)):
        # Only consider ResourceShape subjects that are in the shapes namespace
        if isinstance(s, URIRef) and str(s).startswith(ns):
            for tc in g.objects(s, OSLC.describes):
                if isinstance(tc, URIRef):
                    cname = local_name(tc)
                    shapes[cname] = extract_oslc_shape_properties(g, s)
    # Also support subjects typed as oslc:ResourceShape (require subject in shapes namespace)
    for s in set(g.subjects(RDF.type, OSLC.ResourceShape)):
        if isinstance(s, URIRef) and str(s).startswith(ns):
            for tc in g.objects(s, OSLC.describes):
                if isinstance(tc, URIRef):
                    cname = local_name(tc)
                    shapes[cname] = extract_oslc_shape_properties(g, s)
    return shapes


def extract_shape_properties(g, shape):
    props = []
    # sh:property triple -> blank node with sh:path, sh:name, sh:minCount, sh:maxCount, sh:class, sh:datatype
    for pnode in g.objects(shape, SH.property):
        prop = {}
        path = next(g.objects(pnode, SH.path), None)
        name = next(g.objects(pnode, SH.name), None)
        if path is None:
            continue
        prop['path'] = str(path)
        prop['name'] = str(name) if name else local_name(path)
        minc = next(g.objects(pnode, SH.minCount), None)
        maxc = next(g.objects(pnode, SH.maxCount), None)
        dtype = next(g.objects(pnode, SH.datatype), None)
        cls = next(g.objects(pnode, SH['class']), None)
        prop['min'] = int(minc) if minc else None
        prop['max'] = int(maxc) if maxc else None
        prop['datatype'] = str(dtype) if dtype else None
        prop['class'] = str(cls) if cls else None
        props.append(prop)
    return props


def extract_oslc_shape_properties(g, shape):
    props = []
    # OSLC ResourceShape: oslc:property -> URI of property description
    for p in g.objects(shape, OSLC.property):
        # p may be a URIRef that identifies a property description (rdf:Description)
        prop = {}
        prop_uri = p
        prop['path'] = str(prop_uri)
        # name
        name = first_literal(g, prop_uri, OSLC.name)
        if not name:
            name = local_name(prop_uri)
        prop['name'] = name
        # valueType
        vt = next(g.objects(prop_uri, OSLC.valueType), None)
        if vt is not None:
            prop['valueType'] = str(vt)
        # occurs
        occ = next(g.objects(prop_uri, OSLC.occurs), None)
        if occ is not None:
            prop['occurs'] = str(occ)
        # range
        rng = next(g.objects(prop_uri, OSLC.range), None)
        if rng is not None:
            prop['range'] = str(rng)
        # datatype? some properties may use oslc:datatype (not standard) or have rdf:type xsd
        dtype = next(g.objects(prop_uri, RDF.type), None)
        prop['datatype'] = None
        # description
        desc = first_literal(g, prop_uri, Namespace('http://purl.org/dc/terms/').description)
        if desc:
            prop['description'] = desc
        props.append(prop)
    # Also collect top-level properties (rdf:type oslc:Property) in this namespace that may not be linked to a shape
    for s in set(g.subjects(RDF.type, OSLC.Property)):
        if isinstance(s, URIRef) and str(s).startswith(str(shape).rsplit('#',1)[0]):
            # this heuristic: same document namespace
            if not any(p['path'] == str(s) for p in props):
                prop = {'path': str(s), 'name': first_literal(g, s, OSLC.name) or local_name(s)}
                vt = next(g.objects(s, OSLC.valueType), None)
                if vt is not None:
                    prop['valueType'] = str(vt)
                occ = next(g.objects(s, OSLC.occurs), None)
                if occ is not None:
                    prop['occurs'] = str(occ)
                rng = next(g.objects(s, OSLC.range), None)
                if rng is not None:
                    prop['range'] = str(rng)
                desc = first_literal(g, s, Namespace('http://purl.org/dc/terms/').description)
                if desc:
                    prop['description'] = desc
                props.append(prop)
    return props


def find_or_create_domain_spec(model_tree, namespace_uri, name=None, prefix=None):
    root = model_tree.getroot()
    # domainSpecifications elements have attribute namespaceURI
    for ds in root.findall('.//{*}domainSpecifications'):
        if ds.get('namespaceURI') == namespace_uri:
            return ds
    ds = etree.Element('domainSpecifications')
    if name:
        ds.set('name', name)
    ds.set('namespaceURI', namespace_uri)
    if prefix:
        ds.set('namespacePrefix', prefix)
    root.append(ds)
    return ds


def sync_shapes(model_path, prefix, rdf_path, namespace_uri, dry_run=False):
    g = parse_rdf(rdf_path)
    shapes = extract_shapes(g, namespace_uri)

    model_tree = etree.parse(model_path)
    ds = find_or_create_domain_spec(model_tree, namespace_uri, name=None, prefix='//@domainPrefixes[name=\'' + prefix + '\']')

    # Build map of existing resources by name
    existing_resources = {el.get('name'): el for el in ds.findall('.//{*}resources')}
    existing_rps = {el.get('name'): el for el in ds.findall('.//{*}resourceProperties')}

    for class_name, props in shapes.items():
        if class_name in existing_resources:
            log.info('Updating shape resource: %s', class_name)
            r_el = existing_resources[class_name]
        else:
            r_el = etree.Element('resources')
            r_el.set('name', class_name)
            ds.append(r_el)
            log.info('Added shape resource: %s', class_name)
        # For each property, ensure a resourceProperties exists
        for p in props:
            pname = p.get('name')
            if not pname:
                continue
            if pname in existing_rps:
                rp = existing_rps[pname]
                # We may update occurs/valueType based on min/max/datatype/class
                maybe_set_occurs(rp, p)
                maybe_set_valueType(rp, p)
                log.info('Updated resourceProperty: %s for %s', pname, class_name)
            else:
                rp = etree.Element('resourceProperties')
                rp.set('name', pname)
                maybe_set_occurs(rp, p)
                maybe_set_valueType(rp, p)
                # propertyDefinition linking to vocabulary property's name
                pd = etree.Element('propertyDefinition')
                # href will link to vocabulary.xml; user may adjust label
                href = f"vocabulary.xml#//@vocabularies[label='{prefix}']/@properties[name='{pname}']"
                pd.set('href', href)
                rp.append(pd)
                ds.append(rp)
                log.info('Added resourceProperty: %s for %s', pname, class_name)

    # Warn about extraneous resourceProperties (present but not in shapes)
    all_shape_props = {p.get('name') for props in shapes.values() for p in props if p.get('name')}
    for name in existing_rps.keys():
        if name not in all_shape_props:
            log.warning('Extraneous resourceProperty in model (not in RDF shapes): %s', name)

    if dry_run:
        log.info('Dry run mode; not saving changes to %s', model_path)
        return

    model_tree.write(model_path, encoding='UTF-8', xml_declaration=True, pretty_print=True)
    log.info('Saved updated model to %s', model_path)


def maybe_set_occurs(rp_element, p):
    # determine occurs attribute from min/max or oslc:occurs values
    minc = p.get('min')
    maxc = p.get('max')
    occurs = p.get('occurs')
    if occurs:
        # map OSLC occurs URIs to model strings
        if occurs.endswith('#Exactly-one') or occurs.endswith('#Exactly-one') or occurs.endswith('Exactly-one'):
            rp_element.set('occurs', 'oneOrOne')
            return
        if occurs.endswith('Zero-or-one'):
            rp_element.set('occurs', 'zeroOrOne')
            return
        if occurs.endswith('Zero-or-many'):
            rp_element.set('occurs', 'zeroOrMany')
            return
        if occurs.endswith('One-or-many'):
            rp_element.set('occurs', 'oneOrMany')
            return
    if minc is None and maxc is None:
        return
    if minc == 1 and (maxc is None or maxc == 1):
        rp_element.set('occurs', 'oneOrOne')
    elif minc == 0 and maxc == 1:
        rp_element.set('occurs', 'zeroOrOne')
    elif minc == 0 and (maxc is None or maxc > 1):
        rp_element.set('occurs', 'zeroOrMany')
    elif minc == 1 and (maxc is None or maxc > 1):
        rp_element.set('occurs', 'oneOrMany')


def maybe_set_valueType(rp_element, p):
    if p.get('datatype'):
        rp_element.set('valueType', 'Literal')
    elif p.get('class'):
        rp_element.set('valueType', 'Resource')
    else:
        # leave default
        pass
