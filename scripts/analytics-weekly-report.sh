#!/bin/bash

SNOWGLOBE=$HOME/repos/snowglobe

$SNOWGLOBE/dist/build/snowglobe-analysis/snowglobe-analysis \
  --geoDB $SNOWGLOBE/vendor/GeoLiteCity.dat \
  --events $SNOWGLOBE/data/events.tsv \
  --mode WeekReport \
  | mutt bdamos@vt.edu -s "Analytics Weekly Report for $(date +%Y-%m-%d)"
