# Lyo Domain Model ↔ RDF Sync Tool (SPEC)

## Goal

Create a command-line tool to synchronize RDF vocabularies and OSLC RDF shapes with Lyo Designer domain model XML files.

Behavior:
- Overwrite/update properties/types in the Lyo Designer XML to match the RDF source for the given namespace (prefix).
- Add any new properties or types found in the RDF source.
- Do NOT delete any extra properties or types that already exist in the Lyo XML; instead, print a warning for each extraneous entity and continue.
- Edit Lyo XML files in-place (preserve formatting and unrelated sections).

Supported RDF formats:
- Turtle (.ttl)
- RDF/XML (.rdf, .xml)
- JSON-LD (.jsonld, .json)
- N-Triples (.nt)

Supported Lyo files to edit:
- Vocabulary model files (example: `vocabulary.xml`)
- Domain specification files (example: `oslcDomainSpecifications.xml`)
- Other Lyo Designer XML model files under domain model directories; the tool will search for matching model entries by prefix or namespace.

Command-line interface

Tool module: `lyo_model_sync` (standalone script/module)

Commands:
- `sync vocab --model <model-xml> --prefix <prefix> --rdf <rdf-file> --namespace <namespace-uri>`
- `sync shapes --model <model-xml> --prefix <prefix> --rdf <rdf-file> --namespace <shapes-namespace-uri>`  
  (Important: for `sync shapes` the `--namespace` argument must be the *shapes* namespace, e.g. `https://.../shapes#`) 
- `sync vocab --model <model-xml> --prefix <prefix> --rdf <rdf-file> --namespace <namespace-uri>`  
  (Important: for `sync vocab` the `--namespace` argument must be the *vocabulary* namespace, e.g. `https://.../vocab#`)
Behavior details

1. Parse RDF file using `rdflib`.
   - Load triples and identify terms (classes, properties, labels, comments, ranges, domains, subClassOf, subPropertyOf, etc.) that are in the provided `<namespace-uri>`.
   - Prefer rdfs:label/rdfs:comment annotations when present; when absent, local names are used.

2. Parse the Lyo Designer model XML using `lxml.etree`.
   - Locate the relevant `<vocabulary>` or `<namespace>` block by matching the `prefix` or the configured model element that contains the namespace URI.
   - If no matching block exists, create one in the file in a reasonable default location (e.g., append to the model or a `<vocabularies>` container).

3. Synchronization rules
   - For each entity (class or property) found in RDF under the namespace:
     - If an element representing the entity exists in the Lyo XML, update its attributes and child elements to match RDF (e.g., label, comment, range, domain, datatype) — this is the "overwrite" behavior.
     - If the element does not exist, insert a new element in the appropriate place.
   - For existing elements in the XML that are NOT present in the RDF namespace, **do not delete** them; emit a warning message listing the extraneous entities and their XML locations.

4. XML editing strategy
   - Preserve unrelated parts of the XML document and only change or add elements necessary for synchronization.
   - Keep existing ordering where possible; new elements will be appended in consistent places.
   - Use `lxml` with pretty printing to keep files readable.

Implementation notes

- Language: Python 3.10+.
- Dependencies:
  - rdflib (RDF parsing)
  - lxml (XML parsing/editing)
  - click or argparse (CLI)
  - pytest (tests)
- Packaging/running:
  - The script will be `uv`-runnable (installable as a module / script). We'll provide a simple `pyproject.toml` or shebang wrapper and use PEP 508 inline pip refs in the script as requested (i.e., `#!/usr/bin/env python -m pip ...`? Note: inline pip refs will be specified in a requirements / README). The tool will be runnable via `python -m lyo_model_sync` or `python -m pipx run ...`.

Testing plan

- Unit tests for RDF parsing: small triples in Turtle and JSON-LD, ensure extraction of classes and properties.
- Unit tests for XML editing: small Lyo-style XML fixtures, verify update/add behavior and warnings for extraneous entities.
- Integration tests:
  - Run `sync vocab` against SysML vocabulary files under `C:\src\oslc\misc\sysml-oslc-server\org.oasis.oslcop.sysml.oslc-domain-model\source` and check that `sysmlVocabularies.xml` receives the updated properties and classes and that extraneous entities are warned about.
  - Run `sync shapes` against SysML shapes; sync into `sysmlDomainSpecification.xml`.
  - Run `sync vocab` for the core domain model vocabularies in `C:\src\oslc\lyo\domains\org.eclipse.lyo.tools.domainmodels` and verify output.

Operational examples (as requested)

- Sync vocab:
  `sync vocab --model C:\src\oslc\misc\sysml-oslc-server\org.oasis.oslcop.sysml.oslc-domain-model\sysmlVocabularies.xml --prefix sysml_vocab --rdf C:\src\oslc\misc\sysml-oslc-server\org.oasis.oslcop.sysml.oslc-domain-model\source\SysML Vocabulary-vocab.xml --namespace 'https://www.omg.org/spec/SysML/20230201/vocab#'`

- Sync shapes:
  `sync shapes --model C:\src\oslc\misc\sysml-oslc-server\org.oasis.oslcop.sysml.oslc-domain-model\sysmlDomainSpecification.xml --prefix sysml_sh --rdf C:\src\oslc\misc\sysml-oslc-server\org.oasis.oslcop.sysml.oslc-domain-model\source\SysML Shapes-shapes.xml --namespace 'https://www.omg.org/spec/SysML/20230201/shapes#'`

Risks and open questions

- Exact Lyo Designer XML structure varies between files (different element/attribute names). Implementation will start with a flexible approach: search for elements matching the namespace URI or prefix; if structure is unexpected, the tool will log a diagnostic and skip safe edits.
- Clarify "uv-runnable" meaning (we interpret as a module/script runnable via `python -m` and/or via pipx). If you intended `uvicorn`-based runtime, say so.

Next steps

1. Implement the CLI and core sync behaviour (RDF parsing, XML detecting/updating) with logging and warnings. (Done)
2. Add tests and run the integration tests on the SysML domain model and the core domain model vocabs. (Done: unit tests pass; dry-run integration syncs executed)
3. Iterate on any issues found in integration runs and finalize documentation. (In progress)

Recent findings (integration dry-run)

- SysML vocabulary and shapes were parsed successfully and the tool discovered many classes and properties to update or add in the model. The dry-run reported updates and extraneous entities (warnings) as expected. ✅
- Shapes parsing: RDFlib emitted a number of warnings when attempting to convert XMLLiteral values to DOM (these are data quality / literal format issues in the shapes RDF); the tool proceeds despite these warnings, but we may add more robust XML-literal handling in a follow-up. ⚠️

Next: finish README and packaging, and add a short 'Examples' section (completed) and notes about the XMLLiteral warnings. Then we can open PR or iterate on additional shape-specific mapping rules if you want more precise oslc:property attribute mapping. My next step is to finish documentation examples and provide a short usage guide in `README.md` (done) and prepare a small PR with these changes.

