# idream

A simple build system for Idris


## Current status

It works for simple projects (with local or remote dependencies) but does not
attempt to do anything smart with phases or ordering (i.e. you have to manually
build all dependencies in order). No guarantees on projects with FFI, custom
makefiles, JS targets, or on non-Mac platforms.

Contributions are welcome. Open questions:

* Can we simply replace ipkg files with JSON or YAML files?
* Why is ipkg a custom data format in the first place?
* What is the community process by which we curate package sets?
* Can we reuse "real" build systems like `bazel`?


## Installation and use

Requires `python3`, a recent `idris`, and maybe `git` on your path. Try these:

    # install idream globally
    pip3 install idream

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


## Installation and use

Development requires `virtualenv` on your path. Follow this flow:

    # initialize the virtualenv
    ./scripts/develop.sh

    # lint the project
    ./scripts/lint.sh

    # run the unit tests
    ./scripts/test.sh

    # run idream out of the virtualenv on the demo projects
    ./scripts/demo.sh build demo_lib

    # run all the standard checks
    ./scripts/ci.sh


## Design

The goal is to make it easy to define and build multi-package Idris projects
with external dependencies. To that end, there are definitions for project,
package set, and package configuration files in JSON format that can be used
by any build system. This one just happens to be written in Python and happens
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

Take a look at the `jsonschema` definitions for these files in `idream/idream/schemas`.

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
