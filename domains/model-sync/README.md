# Lyo Model Sync Tool

This tool synchronizes RDF vocabularies and OSLC RDF shapes into Lyo Designer domain model XML files.

## Core domains sync

```sh
uv run --project domains/model-sync --package lyo-model-sync -m lyo_model_sync -- sync-vocab --model "domains/org.eclipse.lyo.tools.domainmodels/vocabulary.xml" --prefix oslc_cm --rdf "C:\src\oslc\misc\oslc-specs\specs\cm\change-mgt-vocab.ttl" --namespace "http://open-services.net/ns/cm#"

uv run --project domains/model-sync --package lyo-model-sync -m lyo_model_sync -- sync-vocab --model "domains/org.eclipse.lyo.tools.domainmodels/vocabulary.xml" --prefix oslc_rm --rdf "C:\src\oslc\misc\oslc-specs\specs\rm\requirements-management-vocab.ttl" --namespace "http://open-services.net/ns/rm#"

uv run --project domains/model-sync --package lyo-model-sync -m lyo_model_sync -- sync-vocab --model "domains/org.eclipse.lyo.tools.domainmodels/vocabulary.xml" --prefix oslc_qm --rdf "C:\src\oslc\misc\oslc-specs\specs\qm\quality-management-vocab.ttl" --namespace "http://open-services.net/ns/qm#"

uv run --project domains/model-sync --package lyo-model-sync -m lyo_model_sync -- sync-vocab --model "domains/org.eclipse.lyo.tools.domainmodels/vocabulary.xml" --prefix oslc_am --rdf "C:\src\oslc\misc\oslc-specs\specs\am\architecture-management-vocab.ttl" --namespace "http://open-services.net/ns/am#"
```

and shapes:

```sh
uv run --project domains/model-sync --package lyo-model-sync -m lyo_model_sync -- sync-shapes --model "domains/org.eclipse.lyo.tools.domainmodels/oslcDomainSpecifications.xml" --prefix oslc_cm_shapes --rdf "C:\src\oslc\misc\oslc-specs\specs\cm\change-mgt-shapes.ttl" --namespace "http://open-services.net/ns/cm/shapes/3.0#"

```

## Intro

### Vocabulary sync (dry-run)
python -m lyo_model_sync sync-vocab --model "C:\src\oslc\misc\sysml-oslc-server\org.oasis.oslcop.sysml.oslc-domain-model\sysmlVocabularies.xml" --prefix sysml_vocab --rdf "C:\src\oslc\misc\sysml-oslc-server\org.oasis.oslcop.sysml.oslc-domain-model\source\SysML Vocabulary-vocab.xml" --namespace "https://www.omg.org/spec/SysML/20230201/vocab#" --dry-run

### Shapes sync (dry-run)
python -m lyo_model_sync sync-shapes --model "C:\src\oslc\misc\sysml-oslc-server\org.oasis.oslcop.sysml.oslc-domain-model\sysmlDomainSpecification.xml" --prefix sysml_sh --rdf "C:\src\oslc\misc\sysml-oslc-server\org.oasis.oslcop.sysml.oslc-domain-model\source\SysML Shapes-shapes.xml" --namespace "https://www.omg.org/spec/SysML/20230201/shapes#" --dry-run

Notes:
- The tool updates existing class/property entries (overwriting label/comment), adds any missing ones, and warns about extraneous entities that exist in the XML but are not present in the RDF - it does NOT delete those.
- Use `--dry-run` to preview changes without modifying files.
- The tool will print warnings for any extraneous entities found in the model that are not present in the RDF (these are NOT deleted automatically).
- `--namespace` for `sync-shapes` must be the *shapes* namespace (the ResourceShape subjects live in this namespace). For vocabulary synchronization use `sync-vocab` with the vocabulary namespace (e.g., `.../vocab#`).
