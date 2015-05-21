#!/bin/bash

set -x -e # Output commands being executed and propogate errors.
cd $(dirname $0)/.. # cd into the snowglobe

./scripts/bootstrap.sh
./scripts/start-collect-enrich.sh &
CE_PID=$!

sleep 1 # Be sure the collector and enricher are initialized.
./sample/send-view.rb # Send a single page view.

SG=./dist/build/snowglobe-analysis/snowglobe-analysis
for MODE in {DaySummary,DayReport,WeekReport}; do
  $SG --events data/events.tsv $MODE
done

kill $CE_PID
