# idream

[![CircleCI](https://circleci.com/gh/idream-build/idream/tree/master.svg?style=svg)](https://circleci.com/gh/idream-build/idream/tree/master)

A simple build system for Idris


## Current status

It works for basic package resolution and compilation.

Notably, it does NOT yet support recursive project fetching, but you probably won't hit that.


## Contributors

* [Eric Conlon](https://github.com/ejconlon)
* [Luc Tielen](https://github.com/luc-tielen)
* ... you? Try it out! File bugs! Send patches!

## Installation and use

The `Makefile` has most of what you need to build and install the `idream` binary:

    make build
    make test
    make integration_test
    make install
    idream --help

`idream --log-level=debug` will crank up the noise.


## Tutorial

Let's start an Idris project from scratch! (NOTE: `tut_project` is git-ignored in this repo so you can
just type these commands safely.)

    idream new tut_project

    # We're now going to cd into the project dir,
    # but you could also pass `--package-dir tut_project`
    # in all the following commands.
    cd tut_project

`idream` creates two files:

* `idr-project.json` - This is a top-level project configuration file. It contains the package name as well
  as subpackage paths.
* `idr-package-set.json` - This contains references to external packges. (We will talk about this later.)

Now we need to add some subpackages. You can add any number of libraries and executables.  (Note that
for now tests are just special executables.)

    # `library` packages can be depended on by other packages
    # They packages are the default, so you can omit `--package-type`
    idream add tut_lib --package-type library

    # `executable` packages are runnable programs
    idream add tut_exe --package-type executable

    # `test` packages are executables that idream gathers and runs with the `test` command
    idream add tut_test --package-type test

Notice that there are now packages listed in the `idr-project.json` file, and some default code has been
generated in those subdirectories. Each package has an `idr-package.json` that is more or less equivalent
to an Idris `ipkg` file. We can fetch all dependencies and build projects like so:

    # Fetch remote dependencies (none so far)
    idream fetch

    # Compile all packages in the project (can specify subset)
    idream compile

    # Run all tests in the project (can specify subset)
    idream test

    # Enter a repl for the given package
    idream repl tut_lib

We hope to make this process easier soon. For now, you can dig around in the `.idream-work` directory and
find compiled libraries and executables, but in the future you'll be able to `run`, `install`, etc.


## Design

The goal is to make it easy to define and build multi-package Idris projects
with external dependencies. To that end, there are definitions for project,
package set, and package configuration files in JSON format that can be used
by any build system. This one just happens to be written in Haskell and happens
to sandbox things in a certain way.

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

TODO(ejconlon) Resurrect `jsonschema` definitions for these files.

### idream specifics

`idream` goes out of its way to keep build and external dependency artifacts out
of source and global directories. When you invoke `idream` it looks for
an `idr-project.json` file in the current directory (or the configured `--project-dir`).
It uses the paths in the project definition to load package definitions.

`idream` then creates an ignorable "cache" directory `.idream-work`
in the same directory as the project file. This acts much like the directory
containing `idris --libdir` and the like, but is local to your project. `idream` stores
executables, compiled libraries, docs, and cloned dependencies in the cache.

When necessary, `idream` will search for package set definitions to resolve
dependencies. Currently, it only looks for a local `idr-package-set.json` file
next to the project file.
