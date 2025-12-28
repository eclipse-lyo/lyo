import logging
from rdflib import Graph, RDF, RDFS, OWL, URIRef, BNode, Namespace
from rdflib.namespace import SH

# status vocabulary: vs:term_status
VS = Namespace('http://www.w3.org/2003/06/sw-vocab-status/ns#')
from lxml import etree
from pathlib import Path
import uuid


def _make_id(prefix='rp'):
    # generate an id similar to existing pattern (underscore + uuid4 hex)
    return '_' + (prefix + '_' + uuid.uuid4().hex[:12])


def _find_resource_by_id(ds, id_val):
    for r in ds.findall('.//{*}resources'):
        if r.get('id') == id_val:
            return r
    return None


def _resource_has_property(ds, resource_el, prop_name):
    rp_list = resource_el.get('resourceProperties')
    if not rp_list:
        return False
    ids = [x for x in rp_list.split() if x.strip()]
    # Search the full model (not just the current domain) so properties attached in other domains are detected
    root = resource_el.getroottree()
    for rid in ids:
        rp_el = root.find('.//{*}resourceProperties[@id="' + rid + '"]')
        if rp_el is not None and rp_el.get('name') == prop_name:
            return True
    return False

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
    if uri is None:
        return None
    s = str(uri)
    if '#' in s:
        return s.split('#')[-1]
    else:
        return s.rstrip('/').split('/')[-1]

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
                term_status = first_literal(g, s, VS.term_status)
                # Use vs:term_status only when it equals 'archaic', otherwise fall back to rdfs:comment
                comment = first_literal(g, s, RDFS.comment)
                if term_status == 'archaic':
                    comment = term_status
                classes[lname] = {'uri': str(s), 'label': label, 'comment': comment}
            # detect property
            if any(t in (RDF.Property, OWL.ObjectProperty, OWL.DatatypeProperty) for t in types):
                label = first_literal(g, s, RDFS.label)
                term_status = first_literal(g, s, VS.term_status)
                # Use vs:term_status only when it equals 'archaic', otherwise fall back to rdfs:comment
                comment = first_literal(g, s, RDFS.comment)
                if term_status == 'archaic':
                    comment = term_status
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
                    shapes.setdefault(cname, []).extend(extract_shape_properties(g, s))
    # OSLC ResourceShapes
    for s in set(g.subjects(OSLC.describes)):
        # Only consider ResourceShape subjects that are in the shapes namespace
        if isinstance(s, URIRef) and str(s).startswith(ns):
            for tc in g.objects(s, OSLC.describes):
                if isinstance(tc, URIRef):
                    cname = local_name(tc)
                    shapes.setdefault(cname, []).extend(extract_oslc_shape_properties(g, s))
    # Also support subjects typed as oslc:ResourceShape (require subject in shapes namespace)
    for s in set(g.subjects(RDF.type, OSLC.ResourceShape)):
        if isinstance(s, URIRef) and str(s).startswith(ns):
            for tc in g.objects(s, OSLC.describes):
                if isinstance(tc, URIRef):
                    cname = local_name(tc)
                    shapes.setdefault(cname, []).extend(extract_oslc_shape_properties(g, s))
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
        # read explicit oslc:valueType on sh:property when present
        vt = next(g.objects(pnode, OSLC.valueType), None)
        if vt is not None:
            prop['valueType'] = str(vt)
        # propertyDefinition: allow sh:property nodes to include oslc:propertyDefinition
        pd = next(g.objects(pnode, OSLC.propertyDefinition), None)
        if pd is not None:
            prop['propertyDefinition'] = str(pd)
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
        # propertyDefinition: a pointer to a vocabulary property (e.g., oslc_cm:closeDate or dcterms:created)
        pd = next(g.objects(prop_uri, OSLC.propertyDefinition), None)
        if pd is not None:
            prop['propertyDefinition'] = str(pd)
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
                pd = next(g.objects(s, OSLC.propertyDefinition), None)
                if pd is not None:
                    prop['propertyDefinition'] = str(pd)
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

    # Build map of existing resources by name and ids
    existing_resources = {el.get('name'): el for el in ds.findall('.//{*}resources')}
    id_to_resource = {el.get('id'): el for el in ds.findall('.//{*}resources') if el.get('id')}
    existing_rps = {el.get('name'): el for el in ds.findall('.//{*}resourceProperties')}

    # Process shapes in ancestor-to-descendant order so parents get properties first
    def shape_depth(name):
        depth = 0
        el = existing_resources.get(name)
        seen = set()
        while el is not None and el.get('extends'):
            if el.get('id') in seen:
                break
            seen.add(el.get('id'))
            parent = _find_resource_by_id(ds, el.get('extends'))
            if parent is None:
                break
            depth += 1
            el = parent
        return depth

    ordered_shapes = sorted(shapes.keys(), key=shape_depth)

    for class_name in ordered_shapes:
        props = shapes.get(class_name) or []
        if class_name in existing_resources:
            log.info('Updating shape resource: %s', class_name)
            r_el = existing_resources[class_name]
        else:
            r_el = etree.Element('resources')
            r_el.set('name', class_name)
            ds.append(r_el)
            existing_resources[class_name] = r_el
            log.info('Added shape resource: %s', class_name)
        # For each property, ensure a resourceProperties exists and attach to the correct resource
        for p in props:
            pname = p.get('name')
            if not pname:
                continue

            # If an ancestor already has this property, skip attaching to current resource
            ancestor = r_el
            parent_id = ancestor.get('extends')
            skip_attach = False
            while parent_id:
                parent_res = id_to_resource.get(parent_id)
                if parent_res is None:
                    break
                if _resource_has_property(ds, parent_res, pname):
                    skip_attach = True
                    break
                ancestor = parent_res
                parent_id = ancestor.get('extends')

            if skip_attach:
                log.info('Ancestor already has property %s; not adding to %s', pname, class_name)
                # still ensure the property exists globally in domain (create or update)
                if pname in existing_rps:
                    rp = existing_rps[pname]
                    maybe_set_occurs(rp, p)
                    maybe_set_valueType(rp, p)
                else:
                    # If a global resourceProperty with this name exists in the same domain, reuse it; otherwise create one local to this domain
                    existing_global = None
                    for r in model_tree.findall('.//{*}resourceProperties'):
                        if r.get('name') == pname and r.get('id'):
                            # ensure this resourceProperty belongs to the same domainSpecifications element (ds)
                            parent = r.getparent()
                            in_same_domain = False
                            while parent is not None:
                                if parent.tag.endswith('domainSpecifications'):
                                    if parent is ds:
                                        in_same_domain = True
                                    break
                                parent = parent.getparent()
                            if in_same_domain:
                                existing_global = r
                                break
                    if existing_global is not None:
                        rp = existing_global
                        maybe_set_occurs(rp, p)
                        maybe_set_valueType(rp, p)
                    else:
                        rp = etree.Element('resourceProperties')
                        rid = _make_id('rp')
                        rp.set('id', rid)
                        rp.set('name', pname)
                        maybe_set_occurs(rp, p)
                        maybe_set_valueType(rp, p)
                        # ensure we always set a valueType for newly created properties (avoid boolean detection)
                        if not rp.get('valueType'):
                            rp.set('valueType', 'Literal')
                        # set range attribute if it maps to an existing resource (support sh:class as well)
                        rng = p.get('range') or p.get('class')
                        if rng:
                            local = local_name(rng)
                            rtarget = existing_resources.get(local)
                            if rtarget is not None and rtarget.get('id'):
                                rp.set('range', rtarget.get('id'))
                            else:
                                # not found: create a resources entry representing that class in this domain and describe it from the vocab
                                rs = str(rng)
                                if rs and ('#' in rs or '/' in rs):
                                    if '#' in rs:
                                        ns = rs.rsplit('#', 1)[0] + '#'
                                        local_cls = rs.rsplit('#', 1)[1]
                                    else:
                                        ns = rs.rsplit('/', 1)[0] + '/'
                                        local_cls = rs.rsplit('/', 1)[1]
                                    # first try to find any existing resources with that name elsewhere in the model
                                    found = None
                                    for r in model_tree.findall('.//{*}resources'):
                                        if r.get('name') == local_cls and r.get('id'):
                                            found = r
                                            break
                                    if found is not None:
                                        rp.set('range', found.get('id'))
                                    else:
                                        # Do not create a resources element for oslc:Any / AnyResource
                                        if local_cls in ('Any', 'AnyResource'):
                                            # ensure valueType indicates a resource reference and leave range unset
                                            if not rp.get('valueType'):
                                                rp.set('valueType', 'Resource')
                                        else:
                                            new_res = etree.Element('resources')
                                            new_id = _make_id('res')
                                            new_res.set('id', new_id)
                                            new_res.set('name', local_cls)
                                            desc = etree.Element('describes')
                                            desc_href = f"vocabulary.xml#//@vocabularies[namespaceURI='{ns}']/@classes[name='{local_cls}']"
                                            desc.set('href', desc_href)
                                            new_res.append(desc)
                                            ds.append(new_res)
                                            existing_resources[local_cls] = new_res
                                            # set range to new id
                                            rp.set('range', new_id)
                    # propertyDefinition href
                    pd = etree.Element('propertyDefinition')
                    if p.get('propertyDefinition'):
                        pd_uri = p.get('propertyDefinition')
                        if '#' in pd_uri:
                            ns = pd_uri.rsplit('#', 1)[0] + '#'
                            local = pd_uri.rsplit('#', 1)[1]
                        else:
                            ns = pd_uri.rsplit('/', 1)[0] + '/'
                            local = pd_uri.rsplit('/', 1)[1]
                        href = f"vocabulary.xml#//@vocabularies[namespaceURI='{ns}']/@properties[name='{local}']"
                    else:
                        prop_path = p.get('path')
                        if prop_path and ('#' in prop_path or '/' in prop_path):
                            if '#' in prop_path:
                                ns = prop_path.rsplit('#', 1)[0] + '#'
                            else:
                                ns = prop_path.rsplit('/', 1)[0] + '/'
                        else:
                            ns = ''
                        local = pname
                        href = f"vocabulary.xml#//@vocabularies[namespaceURI='{ns}']/@properties[name='{local}']"
                    pd.set('href', href)
                    # append propertyDefinition only if not present
                    if not any(child.tag.endswith('propertyDefinition') for child in rp):
                        rp.append(pd)
                    # ensure mapping tracks the global rp
                    existing_rps[pname] = rp
                continue

            # Now, ensure a global resourceProperty exists
            if pname in existing_rps:
                rp = existing_rps[pname]
                maybe_set_occurs(rp, p)
                maybe_set_valueType(rp, p)
                # set range if not already set
                if p.get('range') and not rp.get('range'):
                    rng = p.get('range')
                    local = local_name(rng)
                    rtarget = existing_resources.get(local)
                    if rtarget is not None and rtarget.get('id'):
                        rp.set('range', rtarget.get('id'))
                    else:
                        # try to find a matching resources element anywhere in the model before creating a new one
                        rs = str(rng)
                        if rs and ('#' in rs or '/' in rs):
                            if '#' in rs:
                                ns = rs.rsplit('#', 1)[0] + '#'
                                local_cls = rs.rsplit('#', 1)[1]
                            else:
                                ns = rs.rsplit('/', 1)[0] + '/'
                                local_cls = rs.rsplit('/', 1)[1]
                            found = None
                            for r in model_tree.findall('.//{*}resources'):
                                if r.get('name') == local_cls and r.get('id'):
                                    found = r
                                    break
                            if found is not None:
                                rp.set('range', found.get('id'))
                            else:
                                # Do not create a resources element for oslc:Any / AnyResource
                                if local_cls in ('Any', 'AnyResource'):
                                    if not rp.get('valueType'):
                                        rp.set('valueType', 'Resource')
                                else:
                                    new_res = etree.Element('resources')
                                    new_id = _make_id('res')
                                    new_res.set('id', new_id)
                                    new_res.set('name', local_cls)
                                    desc = etree.Element('describes')
                                    desc_href = f"vocabulary.xml#//@vocabularies[namespaceURI='{ns}']/@classes[name='{local_cls}']"
                                    desc.set('href', desc_href)
                                    new_res.append(desc)
                                    ds.append(new_res)
                                    existing_resources[local_cls] = new_res
                                    rp.set('range', new_id)
                rp_id = rp.get('id')
                log.info('Updated resourceProperty: %s for %s', pname, class_name)
            else:
                # If a global resourceProperty with this name exists in the same domain, reuse it; otherwise create a local rp
                existing_global = None
                for r in model_tree.findall('.//{*}resourceProperties'):
                    if r.get('name') == pname and r.get('id'):
                        parent = r.getparent()
                        in_same_domain = False
                        while parent is not None:
                            if parent.tag.endswith('domainSpecifications'):
                                if parent is ds:
                                    in_same_domain = True
                                break
                            parent = parent.getparent()
                        if in_same_domain:
                            existing_global = r
                            break
                if existing_global is not None:
                    rp = existing_global
                    maybe_set_occurs(rp, p)
                    maybe_set_valueType(rp, p)
                else:
                    rp = etree.Element('resourceProperties')
                    rid = _make_id('rp')
                    rp.set('id', rid)
                    rp.set('name', pname)
                    maybe_set_occurs(rp, p)
                    maybe_set_valueType(rp, p)
                    # ensure we always set a valueType for newly created properties (avoid boolean detection)
                    if not rp.get('valueType'):
                        rp.set('valueType', 'Literal')
                    # set range attr to resource id if we can find the type
                    rng = p.get('range') or p.get('class') or p.get('class')
                    if rng:
                        local = local_name(rng)
                        rtarget = existing_resources.get(local)
                        if rtarget is not None and rtarget.get('id'):
                            rp.set('range', rtarget.get('id'))
                        else:
                            # try to find a resources element anywhere in the model before creating a new one
                            rs = str(rng)
                            if rs and ('#' in rs or '/' in rs):
                                if '#' in rs:
                                    ns = rs.rsplit('#', 1)[0] + '#'
                                    local_cls = rs.rsplit('#', 1)[1]
                                else:
                                    ns = rs.rsplit('/', 1)[0] + '/'
                                    local_cls = rs.rsplit('/', 1)[1]
                                found = None
                                for r in model_tree.findall('.//{*}resources'):
                                    if r.get('name') == local_cls and r.get('id'):
                                        found = r
                                        break
                                if found is not None:
                                    rp.set('range', found.get('id'))
                                else:
                                    # Do not create a resources element for oslc:Any / AnyResource
                                    if local_cls in ('Any', 'AnyResource'):
                                        if not rp.get('valueType'):
                                            rp.set('valueType', 'Resource')
                                    else:
                                        new_res = etree.Element('resources')
                                        new_id = _make_id('res')
                                        new_res.set('id', new_id)
                                        new_res.set('name', local_cls)
                                        desc = etree.Element('describes')
                                        desc_href = f"vocabulary.xml#//@vocabularies[namespaceURI='{ns}']/@classes[name='{local_cls}']"
                                        desc.set('href', desc_href)
                                        new_res.append(desc)
                                        ds.append(new_res)
                                        existing_resources[local_cls] = new_res
                                        rp.set('range', new_id)
                # If no explicit range but valueType indicates resources, do not create an 'Any' resource.
                # Per spec, set the correct valueType and leave range empty so the model indicates a resource reference with unspecified range.
                # propertyDefinition linking
                pd = etree.Element('propertyDefinition')
                pd_uri = p.get('propertyDefinition')
                if pd_uri:
                    if '#' in pd_uri:
                        ns = pd_uri.rsplit('#', 1)[0] + '#'
                        local = pd_uri.rsplit('#', 1)[1]
                    else:
                        ns = pd_uri.rsplit('/', 1)[0] + '/'
                        local = pd_uri.rsplit('/', 1)[1]
                    href = f"vocabulary.xml#//@vocabularies[namespaceURI='{ns}']/@properties[name='{local}']"
                else:
                    prop_path = p.get('path')
                    if prop_path and ('#' in prop_path or '/' in prop_path):
                        if '#' in prop_path:
                            ns = prop_path.rsplit('#', 1)[0] + '#'
                        else:
                            ns = prop_path.rsplit('/', 1)[0] + '/'
                    else:
                        ns = ''
                    local = pname
                    href = f"vocabulary.xml#//@vocabularies[namespaceURI='{ns}']/@properties[name='{local}']"
                pd.set('href', href)
                # append propertyDefinition to rp if not already present
                if not any(child.tag.endswith('propertyDefinition') for child in rp):
                    rp.append(pd)
                # if this resourceProperty was created here, append to domain and mark it; otherwise reuse existing global
                created_here = 'rid' in locals() and rp.get('id') == locals().get('rid')
                if created_here:
                    ds.append(rp)
                    existing_rps[pname] = rp
                    rp_id = rp.get('id')
                    log.info('Added resourceProperty: %s for %s', pname, class_name)
                else:
                    # reuse existing global resourceProperty
                    existing_rps[pname] = rp
                    rp_id = rp.get('id')
                    log.info('Reused global resourceProperty: %s for %s', pname, class_name)

            # Attach the property id to the resource element if not present
            rp_id = rp.get('id')
            existing_rps_attr = r_el.get('resourceProperties')
            existing_list = []
            if existing_rps_attr:
                existing_list = [x for x in existing_rps_attr.split() if x.strip()]
            if rp_id not in existing_list:
                existing_list.append(rp_id)
                r_el.set('resourceProperties', ' '.join(existing_list))
                log.info('Linked resourceProperty %s (%s) to resource %s', pname, rp_id, class_name)

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
    # Prefer explicit oslc:valueType from the shapes when present
    vt = p.get('valueType')
    if vt:
        vts = str(vt)
        # LocalResource preference
        if 'LocalResource' in vts:
            rp_element.set('valueType', 'LocalResource')
            return
        # AnyResource or Resource -> Resource
        if 'Resource' in vts or 'AnyResource' in vts or 'Any' in vts:
            rp_element.set('valueType', 'Resource')
            return
    # Fallbacks
    if p.get('datatype'):
        rp_element.set('valueType', 'Literal')
    elif p.get('class'):
        rp_element.set('valueType', 'Resource')
    else:
        # leave default
        pass
