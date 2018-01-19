#!/usr/bin/env bash

# This script installs the requisite software to run the fuzzer logging
# and summary web service.
#
# This script requires sudo privilege to install virtualenv and required
# system packages.

set -e

HERE=$(cd `dirname $0`; pwd)

$HERE/setup_common.sh

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

# Set up database
sudo -i -u fuzzermon semmc/semmc-fuzzer/fuzzermon/setup_database.sh

# Enable proxying
sudo a2enmod proxy_http

sudo cp -f $HERE/apache.config /etc/apache2/sites-available/fuzzermon.conf

if [ ! -f /etc/apache2/sites-enabled/fuzzermon.conf ]
then
    sudo ln -s /etc/apache2/sites-available/fuzzermon.conf /etc/apache2/sites-enabled/
fi

sudo /etc/init.d/apache2 restart

# Install scripts to deal with gunicorn invocation
sudo cp -f gunicorn.service /etc/systemd/system/
sudo cp -f gunicorn.socket /etc/systemd/system/
sudo cp -f gunicorn.conf /etc/tmpfiles.d/
sudo systemctl enable gunicorn.socket
sudo systemctl start gunicorn.socket
