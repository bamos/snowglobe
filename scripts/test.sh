#!/bin/bash

set -x -e # Output commands being executed and propogate errors.
cd $(dirname $0)/.. # cd into the snowglobe

./scripts/bootstrap.sh
./scripts/start-collect-enrich.sh &
sleep 1
./dist/build/snowglobe-analysis/snowglobe-analysis \
  --events data/events.tsv \
  DayReport

# TODO: Kill collecter and enricher subprocess
