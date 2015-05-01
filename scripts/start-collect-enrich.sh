#!/bin/bash

function die() { echo $*; exit -1; }

cd $(dirname $0)
source env.sh
JAVA_VER=$(java -version 2>&1 | \
  sed 's/java version "\(.*\)\.\(.*\)\..*"/\1\2/; 1q')
[ "$JAVA_VER" -gt 16 ] || die "Java version must be greater than 1.6."

cd ..

stdbuf -i0 -o0 -e0 \
  ./vendor/$COL --config ./conf/collector.conf \
  | ./vendor/$ENR --config ./conf/enrich.conf \
  >> data/events.tsv \
  2>> data/snowglobe-errors.out
