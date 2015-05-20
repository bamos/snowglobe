#!/bin/bash

set -x -e # Output commands being executed and propogate errors.
cd $(dirname $0)/.. # cd into the snowglobe

./scripts/bootstrap.sh
./scripts/start-collect-enrich.sh &
./dist/build/snowglobe-analysis/snowglobe-analysis \
  --events data/events.tsv \
  --mode DayReport

# TODO: Kill collecter and enricher subprocess
