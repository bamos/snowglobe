#!/bin/bash

set -x -e # Output commands being executed and propogate errors.
cd $(dirname $0)/.. # cd into the snowglobe directory

./scripts/bootstrap.sh

sed "s|home/bamos/repos/snowglobe|$PWD|g" \
    conf/enrichments/ip_lookups.json \
    > conf/enrichments/ip_lookups.new.json
mv conf/enrichments/ip_lookups{.new,}.json

#./scripts/start-collect-enrich.sh &
mkdir -p data
stdbuf -i0 -o0 -e0 \
  ./vendor/snowplow-stream-collector-0.4.0 --config ./conf/collector.conf \
  | ./vendor/snowplow-kinesis-enrich-0.5.0 \
    --config ./conf/enrich.conf \
    --resolver file:./conf/resolver.json \
    --enrichments file:./conf/enrichments > data/events.tsv &
CE_PID=$!

sleep 60 # Be sure the collector and enricher are initialized.
./sample/send-view.rb # Send a single page view.

SG=./dist/build/snowglobe-analysis/snowglobe-analysis
for MODE in {DaySummary,DayReport,WeekReport}; do
  $SG --events data/events.tsv $MODE
done

kill $CE_PID
