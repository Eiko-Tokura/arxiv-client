## Arxiv-client

A simple arxiv-client API library for searching, querying, and downloading pdf, source from arXiv.org.

This is a library, and the command line interface (cli) application wrapping this library is provided by the `arxiv-client-cli` package.

## Library Guide

Two query ASTs are defined:

* The primitive `Term`, `ArxivQuery` and their builder functions are defined in `Arxiv.Query`. Suitable for building queries programmatically using functions.

* The higher-level `QueryTerm` which is algebraic query, defined in `Arxiv.Query.Algebraic` and `Arxiv.Query.Parser`, suitable for parsing from user input.
