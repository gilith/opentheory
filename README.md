The opentheory Tool (Development Version)
=========================================

The [opentheory tool][OpentheoryRelease] processes higher order logic theory packages in [OpenTheory][] format.

Cloning this repo will install a [development version][OpentheoryDevelopment], which has active debugging code and includes a large collection of theories used for regressions. The latest official release of the opentheory tool without any extra development cruft [lives here][OpentheoryRelease].

This software is released under the [MIT License][].

Install
-------

Installing the opentheory tool requires the [MLton][], [Poly/ML][] or [Moscow ML][] compiler, as well as standard system tools including GNU Make and Perl.

Clone this repo and initialize the development version:

    git clone https://github.com/gilith/opentheory.git
    cd opentheory
    make init

By default the initialization step requires the [MLton compiler][Mlton], but you can change it to [Poly/ML][] or [Moscow ML][] by editing the top of `Makefile.dev`.

Build
-----

### Using the MLton compiler

Use the [MLton compiler][MLton] to build from source and run the test suite by executing

    make mlton

The opentheory tool executable can then be found at

    bin/mlton/opentheory

### Using the Poly/ML compiler

Use the [Poly/ML compiler][Poly/ML] to build from source and run the test suite by executing

    make polyml

The opentheory tool executable can then be found at

    bin/polyml/opentheory

### Using the Moscow ML compiler

Use the [Moscow ML compiler][Moscow ML] to build from source and run the test suite by executing

    make mosml

The opentheory tool executable can then be found at

    bin/mosml/opentheory

Test
----

A simple test is to display tool help, including the options available for each command:

    path/to/opentheory help

A more serious test is to install the [standard theory library][StandardTheoryLibrary] using the command

    path/to/opentheory install base

Troubleshoot
------------

You can use

    make clean

to clean out any object files.

To report a bug or request an enhancement, please file an issue at [GitHub][OpentheoryIssues].

[OpenTheory]: http://www.gilith.com/opentheory/ "OpenTheory project home page"
[OpentheoryDevelopment]: https://github.com/gilith/opentheory "opentheory tool development"
[OpentheoryIssues]: https://github.com/gilith/opentheory/issues "opentheory tool issues"
[OpentheoryRelease]: http://www.gilith.com/software/opentheory/ "opentheory tool release"
[StandardTheoryLibrary]: http://opentheory.gilith.com/?pkg=base "OpenTheory standard theory library"
[MLton]: http://www.mlton.org/ "MLton compiler"
[Poly/ML]: http://www.polyml.org/ "Poly/ML compiler"
[Moscow ML]: http://www.dina.dk/~sestoft/mosml.html "Moscow ML compiler"
[MIT License]: https://github.com/gilith/opentheory/blob/master/LICENSE "MIT License"
