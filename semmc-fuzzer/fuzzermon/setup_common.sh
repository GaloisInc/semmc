#!/usr/bin/env bash

# This script installs the requisite software to run the fuzzer logging
# and summary web service.
#
# This script requires sudo privilege to install virtualenv and required
# system packages.

set -e

HERE=$(cd `dirname $0`; pwd)

# Core dependencies:
sudo apt-get install --yes python3-pip

# Only because we use an SQLite database and you may want to connect
# manually:
sudo apt-get install --yes sqlite3

# Virtualenv so we can isolate the installation:
sudo pip3 install virtualenv
