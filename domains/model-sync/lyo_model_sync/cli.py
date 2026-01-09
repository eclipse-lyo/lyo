# Inline dependencies: rdflib, lxml, click
import sys
import logging
import os
from pathlib import Path
import click
from .sync import sync_vocab, sync_shapes

logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')
log = logging.getLogger('lyo_model_sync')

@click.group()
def cli():
    """Lyo Designer Model ↔ RDF sync tool"""
    pass

@cli.command('sync-vocab')
@click.option('--model', required=True, type=click.Path(exists=True), help='Path to Lyo vocabulary model XML')
@click.option('--prefix', required=True, help='Prefix used in the model for this namespace')
@click.option('--rdf', 'rdffile', required=True, type=click.Path(exists=True), help='RDF file to import')
@click.option('--namespace', required=True, help='Namespace URI to synchronize')
@click.option('--dry-run', is_flag=True, help='Do not write changes; just print planned changes')
def cmd_sync_vocab(model, prefix, rdffile, namespace, dry_run):
    sync_vocab(model, prefix, rdffile, namespace, dry_run=dry_run)

@cli.command('sync-shapes')
@click.option('--model', required=True, type=click.Path(exists=True), help='Path to Lyo domain specification XML')
@click.option('--prefix', required=True, help='Prefix used in the model for this namespace')
@click.option('--rdf', 'rdffile', required=True, type=click.Path(exists=True), help='Shapes RDF file to import')
@click.option('--namespace', required=True, help='Namespace URI to synchronize')
@click.option('--dry-run', is_flag=True, help='Do not write changes; just print planned changes')
def cmd_sync_shapes(model, prefix, rdffile, namespace, dry_run):
    sync_shapes(model, prefix, rdffile, namespace, dry_run=dry_run)

if __name__ == '__main__':
    cli()
