#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"


GEN='cabal new-run semmc-ppc-genbase --'
DATA="${DIR}/../data"

${GEN} --manual ${DATA}/32/manual \
       --base ${DATA}/32/base \
       --pseudo ${DATA}/32/pseudo \
       --bit-size Size32

${GEN} --manual ${DATA}/64/manual \
       --base ${DATA}/64/base \
       --pseudo ${DATA}/64/pseudo \
       --bit-size Size64
