This program runs programs with initial states on a remote host and reports their results back to the caller.

It is meant to be run via SSH.  Test vectors consist of initial register values, initial memory states, and test programs.  The runner reads test vectors from standard input.  It then uses ptrace to set up the initial states and jumps to the test programs.  It captures the register and memory states after the program is done executing.

For an example of using this code, see ``semmc-x86_64/tools/Main.hs``.

After cross compilation and copying to the remote PPC machine ``helium.proj.galois.com``, this program is the ``--remote-runner EXE`` argument to ``semmc-ppc-stratify`` (see ``semmc-ppc/README.md``).

Limitations:

* Test programs are limited to 4kb
* Test programs cannot contain jumps (or, if they do, they can't jump "outside" of the test, otherwise the final trap won't trigger and we won't be able to stop the process)
* The only backend is currently x86_64 (adding new backends isn't very difficult)
* The x86_64 backend doesn't support floating point, but that only requires a few more ptrace calls
