#!/bin/bash
#
# Download Snowplow and GeoLiteCity binaries in `vendor`.

cd $(dirname $0) # cd into the script's directory.
source env.sh
set -x # Show the commands being executed.

cd ..
mkdir -p vendor
cd vendor

# Download all files if they don't already exist.
[[ -f $COL ]] || wget $CF/$COL_DIR/$COL
chmod a+x $COL
[[ -f $ENR ]] || wget $CF/$ENR_DIR/$ENR
chmod a+x $ENR
if [[ ! -f $SP ]]; then
  wget $CF_2/$SP -O $SP.gz
  gunzip $SP.gz
fi
if [[ ! -f GeoLiteCity.dat ]]; then
  [[ -f GeoLiteCity.dat.gz ]] || wget $GL
  gunzip GeoLiteCity.dat.gz
fi
