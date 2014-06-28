#!/bin/bash

cd $(dirname $0)
source env.sh
set -x

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
