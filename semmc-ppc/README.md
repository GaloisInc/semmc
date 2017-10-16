Executable `semmc-ppc-stratify` (`tools/Stratify.hs`)
======================================

The stratified synthesizer for PPC. Example usage

    cd ~/brittle/semmc.git/tools
    make remote-runner.ppc32
    scp remote-runner.ppc32 helium.proj.galois.com:
    cd ~/brittle/semmc.git/semmc-ppc
    mkdir -p tmp/learned
    stack exec -- semmc-ppc-stratify \
      -r data/iorel \
      -b data/32/base \
      -p data/32/pseudo \
      -l tmp/learned \
      -s tmp/stats.txt \
      -L tmp/log.txt \
      -S 1 \
      -T 10 \
      -H helium.proj.galois.com \
      --remote-runner /home/conathan/remote-runner.ppc32

You can then `tail -f tmp/log.txt` in another shell to track progress.

Instruction Mnemonic Conventions
================================

The instruction names we use are taken from the tablegen file
(`:/submodules/dismantle/dismantle-ppc/data/PPC.tgen`). Where the manual
uses `.` in instruction names we use `o` -- for example our `ADD4o` is `add.`
in the manual -- and where the manual uses `o` we would use `O`, but we
don't have any such instructions at the time of writing. No idea what
the `4` means in `ADD4o`.

PPC Arch Manuals
================

There are many PPC manuals and I don't know if any are preferred. Here
are two. The first is specific to 32-bit PPC:

https://www.nxp.com/docs/en/reference-manual/MPCFPE32B.pdf

The second covers both 32 and 64-bit PPC:

https://www.ibm.com/developerworks/systems/library/es-archguide-v2.html
