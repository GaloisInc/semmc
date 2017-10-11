semmc-ppc-stratify (tools/Stratify.hs)
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
