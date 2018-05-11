This directory contains pairs of files with extensions ".info" and ".bytes". The two
are in correspondence; the .info file is a text file where each line of
pseudo-assembly corresponds to a single instuction in the .bytes file. The .bytes
file contains ARM/Thumb instructions. I've done this for convenience so it's easy to
view these instructions in psittacine in order to examine how they are disassembled
by dismantle.

* My methodology for adding semantics

In ARM/Thumb, each instruction typically comes with several variants. Instructions
vary in whether one operand is an immediate or a register identifier, and even within
those, many different versions of the same instruction can be represented in many
different ways (especially for Thumb).

On top of this, there is an added layer of confusion in that the LLVM TableGen data
used by dismantle for decoding does not map each instruction variant in an one-to-one
fashion to the Dismantle representation. It is not typically clear from glancing at
the Dismantle.ARM/Thumb constructors which Opcode corresponds to which instruction
variant (again, this is especially tricky with Thumb).

Therefore, in order to ensure we get this mapping correct, and that we capture all
variants of each instruction type, I have developed an ad-hoc methodology that gives
me confidence I have done a thorough job of implementing the semantics of each
Dismantle Opcode.

1. Create a .info file in this directory

I create a text file with the name of the instruction followed by A or T, depending
on whether I am looking at the ARM or Thumb variants of that instruction. Then, using
the ARM manual as a guide, I write down a long list of assembly language instructions
that is intended to capture all the different relevant possibilities of each
instruction variant. Sometimes, the exact same variant will get decoded to different
Dismantle constructors, so sometimes it's necessary to use different field values in
order to get the coverage needed to touch all the relevant Opcodes.

2. Manually translate the .info file to a .bytes file

I use emacs hexl-mode to manually enter in the instructions, byte by byte, into a
binary file. This could probably be done automatically with an assembler, but I
haven't bothered to do that yet; the files have been short enough where it doesn't
take me much time to do it manually. 

3. Use psittacine to decode the .bytes file

psittacine has support for non-ELF files in the .bytes format. I load it up
with this command:

psittacine --tui adcA.bytes

or whatever file I'm looking at. Then, I can step through the file and
examine how each variant of the instruction gets decoded, and keep notes on
that as a "comment" in the corresponding .info file. This makes it easy to
know that I've implemented every variant in the correct way.
