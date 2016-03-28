#!/bin/bash

function die() { echo $*; exit -1; }

cd $(dirname $0)
source env.sh
JAVA_VER=$(java -version 2>&1 | \
  sed 's/.* version "\(.*\)\.\(.*\)\..*"/\1\2/; 1q')
[ "$JAVA_VER" -gt 16 ] || die "Java version must be greater than 1.6."

mkdir -p ../data
cd ../data

stdbuf -i0 -o0 -e0 \
  ../vendor/snowplow-stream-collector-0.4.0 --config ../conf/collector.conf \
  | ../vendor/snowplow-kinesis-enrich-0.5.0 \
    --config ../conf/enrich.conf \
    --resolver file:../conf/resolver.json \
    --enrichments file:../conf/enrichments \
  >> events.tsv \
  2>> snowglobe-errors.out
