#!/bin/bash

SNOWGLOBE=$HOME/repos/snowglobe

$SNOWGLOBE/dist/build/snowglobe-analysis/snowglobe-analysis \
  --events $SNOWGLOBE/data/events.tsv \
  DayReport \
  | mutt bdamos@vt.edu -s "Analytics Report for $(date +%Y-%m-%d)"
