Useful for understanding the development of the modern PowerPC ISA: [Power ISA Evolution](https://upload.wikimedia.org/wikipedia/commons/3/3b/PowerISA-evolution.svg).

# FP/Vector Registers on PPC #

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
