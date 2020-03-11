#!/bin/bash

cabal v2-build semmc-asl -f asl-lite
cabal v2-test semmc-asl-test-formulas -f asl-lite
