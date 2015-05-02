#!/bin/bash

SNOWGLOBE=$HOME/repos/snowglobe

$SNOWGLOBE/dist/build/snowglobe-analysis/snowglobe-analysis \
  --geoDB $SNOWGLOBE/vendor/GeoLiteCity.dat \
  --events $SNOWGLOBE/data/events.tsv \
  --mode DailyReport \
  | mutt bdamos@vt.edu -s "Analytics Report for $(date +%Y-%m-%d)"
