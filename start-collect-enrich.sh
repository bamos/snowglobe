#!/bin/bash

function die() { echo $*; exit -1; }

cd $(dirname $0)
source env.sh
JAVA_VER=$(java -version 2>&1 | \
  sed 's/java version "\(.*\)\.\(.*\)\..*"/\1\2/; 1q')
[ "$JAVA_VER" -gt 16 ] || die "Java version must be greater than 1.6."

./$COL --config collector.conf | ./$ENR --config enrich.conf >> events.tsv
