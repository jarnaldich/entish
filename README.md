Description
===========

A racket library / DSL for creating, checking and removing directory
trees. The idea is to have an s-expression-based DSL in which the
nesting level of an s-expression parallels the nesting level in a
directory structure, so that it is easy to see the directory tree that
is to be created/checked/removed. This may be useful, for example, for
scaffolding a directory structure for a development project (maybe a
framework).


Design goals:

- Code structure should mimic directory structure.
-

; 1. Estructura del codi hauria de semblar-se visualment a l'estructura de directoris.
; 2. Hauria de permetre execució de la còpia, però també anàlisi de l'estructura, dependències, etc...
; 3. La descripció de l'estructura hauria de ser independent de l'acció: una mateixa estructura es pot comprovar, generar, actualitzar, documentar...
; 4. Ser capaç d'incorporar funcions de "racket" pures de manera agradable, per quan cal més artilleria.


; Els nodes corresponent als roots, file i dir actualitzen un pàmetre que dóna un "current path", de fet afegeixen un component al final i segueixen avaluant els fills.
; Els nodes generadors de contingut (copy-from, zip, template-string ...) són funcions que agafen el current-path com a sortida i fan el que calgui...

XXX: Update to newer version

Running
=======

Can run through raco, like: http://github.com/jessealama/argo . 

Syntax
======

Starts with ``forest`` , then ``roots``, ...

Nodes
=====

- Regular functions.
- Nodes take breadcrumb as first argument, then a list of chunks corresponding
  to their yet to be eveluated children nodes and are expected to return an
  artifact.


## The ``root`` node

It is always the starting, outmost node in Entish. It denotes that all
sub-expressions describe a directory tree in Entish. The root node can
take two parameters: ``path`` and ``mode``.

* ``path`` stands for the root path of the entish expression. The
  default being ``“.”``, the current directory.
* ``mode`` stands for the actual operation to be performed. Can be
  ``build``, ``test`` or ``remove``.

``build`` creates the directory tree and files anew from the path in root.
``remove`` will delete the directory tree.
``test`` will check that the directory tree under root matches the
given specification.

### The ``dir`` node 

Specifies a sub-directory, its name given in the #:name
parameter. Optionally, a generator of elements can be given in the
``foreach`` argument to create a set of directories. See generators
below.

### The ``file`` node

Specifies a file, and can only have strings as children (no other
files or dirs). In build mode, these strings will be the contents of
the file. In test mode, will be used as a perl-compatible regular
expression to test for the file contents.

The name of the file will be taken form the ``#:name`` parameter,
unless there is a ``foreach`` argument to create a set of files. see
generators below.


Files
=====

The library (collection) and corresponding package lie in the ``entish-lib``` subdir.
Collection name is ``entish``. You can download and install it
by running ``raco pkg install`` from the ``entish`` subdir.

* ``entish/`` is the library collection, entry point at main.rkt so that require
  loads it with directory (collection) name.

  generators that only depend on Racket libraries.
* ``tests.rkt`` contains the unit tests.
* ``main.rkt`` contains an example command-line program using the library.

Development
===========

After installing the package ``entish-lib`` by running ``raco pkg install`` from
the ``entish-lib`` subdir.

Run tests with ``raco test entish-test``
Or continuosly:
``watchexec --exts rkt raco test entish-test ``

Deploy
======
I've had some success running:

```
raco exe entish-lib/command.rkt
raco distribute <deploy_dir> entish-lib/command.exe
```

To generate a standalone executable that works even on a different machine (same
OS).


Status and future plans
=======================

The library is still very rough: API may change and has barely been
tested. It fits my needs so far, but I accept suggestions as where to
go in the future. Some ideas are:

TODO
----

- check should compare timestamps …
- if-newer overwrite mode


Improvements
------------
- Use ``syntax/parse`` for better error reporting from the macros.
- Revise return types to aggregate errors or info on dependencies
- Revise exception handling in nodes
- Centralize logging/msg module

Niceties
--------
- Support for non-destructive operations (ie. being able to build, check results and rollback if something goes wrong or something of the like). Could be done with a "safe" parameter that makes destructive nodes (clear, overwriting copies) to do a noop.
- Iterators
- Best effort vs fail first
- Dependency analysis.
- Async/concurrent execution of nodes.
- Support for compilation of scripts from the ``raco entish`` command.
- Support for "grafts": embedding a tree from another file.
- Generalized roots / file systems: ftp, webdav (crazier things, like dbs) 
- Alternative syntax (indentation-based text template that can fall back to racket when needed)
- More / Better nodes (iterators)

Resources
=========

- [Package Tutorial](https://blog.racket-lang.org/2017/10/tutorial-creating-a-package.html)
- [Pict Package](https://github.com/racket/pict)
- [Fear of Macros]()
