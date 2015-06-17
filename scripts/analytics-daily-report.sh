#!/bin/bash

SNOWGLOBE=$HOME/repos/snowglobe

$SNOWGLOBE/dist/build/snowglobe-analysis/snowglobe-analysis \
  --events $SNOWGLOBE/data/events.tsv \
  DayReport \
  | mutt brandon.amos.cs@gmail.com -s "Analytics Report for $(date +%Y-%m-%d)"
