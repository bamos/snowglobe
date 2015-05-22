#!/bin/bash

set -x -e # Output commands being executed and propogate errors.
cd $(dirname $0)/.. # cd into the snowglobe directory

./scripts/bootstrap.sh

sed "s|home/bamos/repos/snowglobe|$PWD|g" \
    conf/enrichments/ip_lookups.json \
    > conf/enrichments/ip_lookups.new.json
mv conf/enrichments/ip_lookups{.new,}.json

./scripts/start-collect-enrich.sh &
CE_PID=$!

# Be sure the collector and enricher are initialized.
# On real servers, initialization happens in subsecond time.
# Travis' servers need a longer delay due to compute/memory limitations.
sleep 60

# Send a single page view.
./sample/send-view.rb

kill $CE_PID

# Make sure the events file is flushed to disk.
sleep 5

SG=./dist/build/snowglobe-analysis/snowglobe-analysis
$SG --events data/events.tsv DaySummary
$SG --events data/events.tsv DayReport
$SG --events data/events.tsv WeekReport
