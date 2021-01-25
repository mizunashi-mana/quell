# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
# import os
# import sys
# sys.path.insert(0, os.path.abspath('.'))


# -- Project information -----------------------------------------------------

project = 'quell-spec'
copyright = '2020, Mizunashi Mana'
author = 'Mizunashi Mana'


# -- General configuration ---------------------------------------------------

extensions = [
    'sphinx.ext.githubpages',
    'sphinxcontrib.katex',
    'sphinxcontrib.bibtex',
]

bibtex_bibfiles = ['reference.bib']

templates_path = ['_templates']

source_suffix = ['.rst']

master_doc = 'index'

exclude_patterns = []


# -- Options for HTML output -------------------------------------------------

html_theme = 'alabaster'

html_theme_options = {
  'github_user': 'mizunashi-mana',
  'github_repo': 'quell',
  'github_banner': True,
}

html_static_path = ['_static']
