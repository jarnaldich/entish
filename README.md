Description
===========

A racket library / DSL for creating, checking and removing directory
trees. The idea is to have an s-expression-based DSL in which the
nesting level of an s-expression parallels the nesting level in a
directory structure, so that it is easy to see the directory tree that
is to be created/checked/removed. This may be useful, for example, for
scaffolding a directory structure for a development project (maybe a
framework).

For example, the following code:

<pre>
(root #:path “~/code/projects/erlang/sample”
      #:mode ‘build
      (dir #:name “ebin”
           (file #:name “sample.app”
                “{application, Sample, []}”)
      (dir #:name “src”)
      (dir #:name “priv”))
</pre>

Would create three dirs named ``src``, ``priv`` and ``ebin`` under
``~/code/projects/erlang/sample``, and a file under the ``ebin`` with
some sample text.

Nodes
=====

There are three kind of nodes in Entish: a single root, nested dirs
and final files. Every node can take some optional parameters
(starting with ``#:``) affecting its behaviour, and a list of child
objects (other nodes or strings), which go one level deeper in the
described directory structure.

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

Generators
==========

The ``file`` and ``dir`` nodes can both take a ``foreach`` argument,
which will take a regular Racket generator as an argument. Instead of
creating, checking or removing just one file, the operation will be
repeated as long as the generator does not return ``void`` or ``eof``.

The generator can return many ``values``, which will be interpolated
into the ``name`` parameter. A ``?n`` in the string passed as a name
will be replaced with the nth value returned by the generator. 

As an example (taken from the tests), the following code will create a
file with "Sample text" as its contents named after each line read
from stdin.

    (root #:path (find-system-path 'temp-dir)
        #:mode 'build
        (file #:name "?0.txt"
              #:foreach stdin-lines
              "Sample text"))


Entish comes with a set of pre-defined generators so that the code can
be kept compact and descriptive. For example, the ``stdin-lines``
above will return a value for each line read from ``stdin``.


Files
=====

* ``entish.rkt`` is the main module. It provides the nodes and
  generators that only depend on Racket libraries.
* ``tests.rkt`` contains the unit tests.
* ``main.rkt`` contains an example command-line program using the library.

Status and future plans
=======================

The library is still very rough: API may change and has barely been
tested. It fits my needs so far, but I accept suggestions as where to
go in the future. Some ideas are:

* Include a xml reader that allows the specs to be in an XML file.
* Change reporting of errors and information.     

