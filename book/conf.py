# -*- coding: utf-8 -*-

from recommonmark.parser import CommonMarkParser
source_parsers = {
    '.md': CommonMarkParser,
}

extensions = []
templates_path = ['_templates']
source_suffix = ['.rst', '.md']
master_doc = 'index'

# General information about the project.
project = u'Das tttool-Buch'
copyright = u'2017, Joachim Breitner'
author = u'Joachim Breitner'

version = u'1.7.0'
release = u'1.7.0'

language = 'de'

exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']
pygments_style = 'sphinx'
todo_include_todos = False

# -- Options for HTML output ----------------------------------------------
#html_theme = 'alabaster'
html_static_path = ['_static']

# -- Options for HTMLHelp output ------------------------------------------

htmlhelp_basename = 'tttool'

# -- Options for LaTeX output ---------------------------------------------

latex_elements = {
}

# Grouping the document tree into LaTeX files. List of tuples
# (source start file, target name, title,
#  author, documentclass [howto, manual, or own class]).
latex_documents = [
    (master_doc, 'tttool.tex', u'Das tttool-Buch',
     u'Joachim Breitner', 'manual'),
]


# -- Options for manual page output ---------------------------------------

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).
man_pages = [
    (master_doc, 'tttool', u'Das tttool-Buch',
     [author], 1)
]


# -- Options for Texinfo output -------------------------------------------

texinfo_documents = [
    (master_doc, 'tttool', u'Das tttool-Buch',
     author, 'tttool', 'Tiptoi zum selberbasteln',
     'Miscellaneous'),
]



# -- Options for Epub output ----------------------------------------------

# Bibliographic Dublin Core info.
epub_title = project
epub_author = author
epub_publisher = author
epub_copyright = copyright
epub_identifier = 'http://tttool.entropia.de'

# A unique identification for the text.
#
# epub_uid = ''

# A list of files that should not be packed into the epub file.
epub_exclude_files = ['search.html']
