#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"


GEN="cabal new-run semmc-ppc-genbase -- "
DATA="${DIR}/../data"

${GEN} --manual ${DATA}/32/manual \
       --base ${DATA}/32/base \
       --pseudo ${DATA}/32/pseudo \
       --bit-size Size32

${GEN} --manual ${DATA}/64/manual \
       --base ${DATA}/64/base \
       --pseudo ${DATA}/64/pseudo \
       --bit-size Size64

# Examine any changes by building cmpsem from semmc itself and running:
# for Z in 32 64; do for Y in manual pseudo base; do for X in data/$Z/$Y/*.sem; do cmpsem <(git cat-file blob :semmc-ppc/$X) $X; done; done; done | grep -v 'semantically equivalent'
