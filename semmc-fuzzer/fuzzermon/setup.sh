#!/usr/bin/env bash

# This script installs the requisite software to run the fuzzer logging
# and summary web service.
#
# This script requires sudo privilege to install virtualenv and required
# system packages.

set -e

HERE=$(cd `dirname $0`; pwd)
ENV=$HERE/ENV

sudo apt-get install --yes python3-pip sqlite3
sudo pip3 install virtualenv

virtualenv $ENV
source $ENV/bin/activate

pip install Django
