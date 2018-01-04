# idream

A simple build system for Idris


## Current status

It works for simple projects but has trouble installing dependencies to a
custom path, a [known issue](https://github.com/idris-lang/Idris-dev/issues/3383).


## Installation and use

Requires `python3`, `idris`, and `git`. Development requires `virtualenv`.

When it's all packaged, `pip3 install idream` should be enough to get idream on
your path. Crawl the docs with `idream --help`.

If you want to develop locally, initialize the virtualenv with `./scripts/develop.sh`.
You can try out demo builds with `./scripts/demo.sh`, and lint and test with other scripts.

## Design

The goal is to make it easy to define and build multi-package Idris projects
with external dependencies. To that end, there are definitions for project,
package set, and package configuration files in JSON format that can be used
by any build system. This one just happens to be written in Python and happens
to sandbox things in a certain way.
