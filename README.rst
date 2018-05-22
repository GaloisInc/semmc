Overview
========

This repository contains implementations of, and tools supporting, the Semantics of Machine Code (SemMC).

Beyond machine code semantics, two different approaches to synthesis are implemented.  The first is *stratified synthesis* (based on a 2016 paper [PLDI16]_), which learns the semantics of machine code instructions from a manually-specified base set.  The second is a search-based synthesis of machine code programs from semantics using a technique called Counter-Example Guided Inductive Synthesis (CEGIS) [TR1814]_, [OOPSLA16]_.

Repository Layout
-----------------

* *semmc*  is the core library that implements some shared data types, including representations of machine code semantics at different levels of abstraction.  It also implements stratified synthesis and search-based synthesis in an architecture-independent manner.
* *semmc-coverage* is a tools to measure instruction set coverage in our architecture-specific backends.
* *semmc-fuzzer* is a concrete fuzzer to compare our semantics against real hardware.
* *semmc-ppc* is the instantiation of semmc for the PowerPC architecture; it supports both 32 and 64 bit PowerPC.
* *semmc-arm* is the instantiation of semmc for the AArch32 architecture; it supports both ARM and Thumb encodings.
* *semmc-toy* is a simple demonstration architecture that shows what is required of an implementation, and is useful for testing synthesis.
* *tools* contains a C program called the *remote-runner*, which can be used to execute machine code on remote machines (used for running concrete test cases on remote hardware e.g., and ARM or PowerPC machine).


Building
--------

The dependencies of the project that are not on Hackage are specified using git submodules.  To build the code with a modern version of ``cabal`` (assuming you are in the root of the repository)::

  $ git submodule update
  $ ln -s cabal.project.newbuild cabal.project
  $ cabal new-configure
  $ cabal new-build semmc-ppc

Status
======

This codebase is a work in progress.  PowerPC support (both 32 and 64 bit) is reasonably robust.  Support for ARM (AArch32) is on-going.  Eventually, we will add support for x86_64, MIPS, and RISC-V.


License
=======

This code is made available under the BSD3 license and without any support.

References
==========

.. [PLDI16] https://cs.stanford.edu/people/eschkufz/docs/pldi_16.pdf
.. [TR1814] http://research.cs.wisc.edu/wpis/papers/tr1814.pdf
.. [OOPSLA16] http://pages.cs.wisc.edu/~venk/papers/oopsla16b.pdf
