Overview
========

This package implements the PowerPC backend for SemMC (both 32 and 64 bit).

Tools
=====

There are a number of tools built with this library:

 * *semmc-ppc-iorels* is a tool for learning which operands are inputs and which are outputs (which is information that feeds into the stratified synthesis process).
 * *semmc-ppc-stratify* is a driver for starting stratified synthesis for the PowerPC architecture.
 * *semmc-ppc-synthdemo* is a demonstration of machine code synthesis for PowerPC.
 * *semmc-ppc-genbase* executes the semantics DSL definitions to generate textual representations of the PowerPC semantics; there is a helper script in `:/scripts` to supply the necessary arguments to this executable.

Instruction Mnemonic Conventions
================================

The instruction names we use are taken from the tablegen file
(`:/submodules/dismantle/dismantle-ppc/data/PPC.tgen`), with overrides
in `:/submodules/dismantle/dismantle-ppc/data/override/*.tgen`. Where
the manual uses `.` in instruction names we use `o`, and where the
manual uses `o` in instruction names we use `O`. For example, `add.`
in the manual is `ADD4o` in `data/PPC.tgen`, and `addo.` in the manual
is `ADD4Oo` in `data/override/ADD4Oo.tgen` (used by SFE's Trap Arith
Overflows transform). No idea what the `4` means in `ADD4*`.

Semantics Definitions
=====================

The semantics are defined in `:/src/SemMC/Architecture/PPC` using the DSL defined in `SemMC.DSL`.  These definitions mirror the ISA manual as closely as possible.

The semantics get serialized to disk in `:/data/**/.sem` when you run `:/scripts/gen-base.sh`.

PPC Arch Manuals
================

There are many PPC manuals and I don't know if any are preferred. Here
are two. The first is specific to 32-bit PPC:

https://www.nxp.com/docs/en/reference-manual/MPCFPE32B.pdf

The second covers both 32 and 64-bit PPC:

https://www.ibm.com/developerworks/systems/library/es-archguide-v2.html

PPC History
===========

Useful for understanding the development of the modern PowerPC ISA: [Power ISA Evolution](https://upload.wikimedia.org/wikipedia/commons/3/3b/PowerISA-evolution.svg).

At some point, floating point was added to PowerPC.
This original specification added thirty-two 64-bit registers, named FPR0-31.
These support various scalar floating-point operations, on both doubles and singles.

In 1999, the first vector extensions to PowerPC were published, named AltiVec.
This added thirty-two 128-bit registers, named VR0-31.
These support various SIMD integer and floating-point operations.
This was unified with the standard Power ISA spec in v2.03, though it is an optional extension.

In 2009, with Power ISA v2.06, the "Vector-Scalar Floating Point" extension (VSX) was added.
This "added" sixty-four 128-bit registers, named VSR0-63.
However, the first doublewords of VSR0-31 are aliased with FPR0-31, and the all of VSR32-63 are aliased with VR0-31.
