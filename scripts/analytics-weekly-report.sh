#!/bin/bash

SNOWGLOBE=$HOME/repos/snowglobe

$SNOWGLOBE/dist/build/snowglobe-analysis/snowglobe-analysis \
  --events $SNOWGLOBE/data/events.tsv \
  WeekReport \
  | mutt bdamos@vt.edu -s "Analytics Weekly Report for $(date +%Y-%m-%d)"
