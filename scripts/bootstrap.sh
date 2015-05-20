#!/bin/bash
#
# Download Snowplow and GeoLiteCity binaries in `vendor`.

cd $(dirname $0) # cd into the script's directory.
source env.sh
set -x -e # Show the commands being executed and fail if a subprocess fails.

cd ..
rm -rf vendor
mkdir vendor
cd vendor

wget $RELEASE_ZIP
unzip $(basename $RELEASE_ZIP)
rm $(basename $RELEASE_ZIP)

wget $JS_TRACKER

wget $GL
gunzip $(basename $GL)
