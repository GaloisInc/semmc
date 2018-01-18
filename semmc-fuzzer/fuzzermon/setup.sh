#!/usr/bin/env bash

# This script installs the requisite software to run the fuzzer logging
# and summary web service.
#
# This script requires sudo privilege to install virtualenv and required
# system packages.

set -e

HERE=$(cd `dirname $0`; pwd)
ENV=$HERE/ENV

# Set up the environment and activate it in this shell:
virtualenv $ENV
source $ENV/bin/activate

# Install python dependencies:
pip install Django
