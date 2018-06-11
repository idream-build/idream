# idream

[![Build Status](https://travis-ci.org/ejconlon/idream.svg?branch=master)](https://travis-ci.org/ejconlon/idream)

A simple build system for Idris


## Team

* [Luc Tielen](https://github.com/luc-tielen)
* [Eric Conlon](https://github.com/ejconlon)
* ... you? Send patches!


## Current status

TODO(ejconlon) Give an accurate report.


## Installation and use

The `Makefile` has most of what you need to build and install the `idream` binary:

    make build
    make test
    make integration_test
    make install

Once you have it installed, you can use `idream` like this:

    # take a look at the argparse options
    idream --help

    # play with the demo projects
    cd demo
    idream validate
    idream build demo_lib
    idream mkdoc demo_lib
    idream build demo_bin
    idream execute demo_bin
    idream build lightyear

`idream --log-level=DEBUG` will crank up the noise.


## Design

The goal is to make it easy to define and build multi-package Idris projects
with external dependencies. To that end, there are definitions for project,
package set, and package configuration files in JSON format that can be used
by any build system. This one just happens to be written in Haskell and happens
to sandbox things in a certain way.

TODO(ejconlon) Make sure this is still accurate.

### General metadata

To start with, we call a single library or executable corresponding to a single
`ipkg` file a `package`. We can put some subset of what would be in an `ipkg` file
into an `idr-package.json` file in a directory where the `ipkg` would ordinarily be.

We call a collection of `packages` a `project`. We can throw an `idr-project.json`
file at the root of a repository (or in the same directory as a single project)
to list the relative paths to the packages exist in this project.

Drawing insipiration from Purescript's [package-sets](https://github.com/purescript/package-sets),
we can define our own `package set` to resolve libraries to specific refs and subdirectories in
specific git repositories. One might maintain this list in-repository in an
`idr-package-set.json` file or depend on a shared collection.

Take a look at the `jsonschema` definitions for these files in `idream/idream/schemas`.
(TODO(ejconlon) Resurrect these or give some other definition.)

### idream specifics

`idream` goes out of its way to keep build and external dependency artifacts out
of source and global directories. When you invoke `idream` it looks for
an `idr-project.json` file in the current directory (though you can point it
elsewhere). It uses the package paths in the project to load all package
definitions in the project. (Note that local package names always shadow remote ones.)

`idream` then creates an ignorable "cache" directory `.idream` (also configurable)
in the same directory as the project file. This acts much like the directory
containing `idris --libdir` and the like, but is local to your project. `idream` stores
executables, compiled libraries, docs, and cloned dependencies in the cache.

When necessary, `idream` will search for package set definitions to resolve
dependencies. Currently, it only looks for a local `idr-package-set.json` file
next to the project file.
