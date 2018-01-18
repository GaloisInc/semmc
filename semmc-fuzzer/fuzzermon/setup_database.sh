#!/usr/bin/env bash

set -e

HERE=$(cd `dirname $0`; pwd)

. $HERE/ENV/bin/activate
cd fuzzermon && \
    python manage.py migrate
