#!/bin/bash
#
# Generate the data/**/.sem semantics files.
#
# Requires that 'semmc-ppc-genbase' is on your PATH, so you probably
# want to use 'cabal v2-exec' or 'stack exec'.

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"


GEN=$(which semmc-ppc-genbase)
DATA="${DIR}/../data"

if ! which semmc-ppc-genbase &>/dev/null; then
  echo "No $GEN on PATH!" > /dev/stderr
  echo "Try 'cabal v2-exec $0' or 'stack exec $0'."
  exit 1
fi

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
