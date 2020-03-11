#!/bin/bash

cabal v2-build semmc-asl
cabal v2-test semmc-asl-test-formulas
