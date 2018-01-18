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

# Install Postgres and Apache
sudo apt-get install --yes postgresql apache2

# Create user account to run the service
if ! grep fuzzermon /etc/passwd >/dev/null
then
    sudo adduser --disabled-password --gecos "Fuzzermon,,,," fuzzermon
fi

# Create a postgres role for the new user
(sudo -u postgres createuser -d fuzzermon || true)

# Clone the project sources from the tree where we are running this script
REPO=$(cd $HERE/../../ && pwd)
if [ ! -d ~fuzzermon/semmc ]
then
    sudo -i -u fuzzermon git clone $REPO
fi

# Set up the project environment in the fuzzermon account
sudo -i -u fuzzermon semmc/semmc-fuzzer/fuzzermon/setup.sh

# Install bashrc
sudo cp $HERE/fuzzermon_bashrc ~fuzzermon/.bashrc
sudo chown fuzzermon: ~fuzzermon/.bashrc
