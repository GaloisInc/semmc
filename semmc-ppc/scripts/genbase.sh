#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"


GEN='/Users/benselfridge/macaw-semmc/submodules/semmc/semmc-ppc/.stack-work/dist/x86_64-osx/Cabal-2.0.0.2/build/semmc-ppc-genbase/semmc-ppc-genbase'
DATA="${DIR}/../data"

${GEN} --manual ${DATA}/32/manual \
       --base ${DATA}/32/base \
       --pseudo ${DATA}/32/pseudo \
       --bit-size Size32

${GEN} --manual ${DATA}/64/manual \
       --base ${DATA}/64/base \
       --pseudo ${DATA}/64/pseudo \
       --bit-size Size64
