# Set JAVA_HOME (and possibly other Java-related variables) if your
# default JDK is not supported by Snowplow.
#export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_05.jdk/Contents/Home

# Separate the variables this much to avoid duplication and
# for easier access in the other scripts.

# Snowplow Scala collector and enricher JARs.
CF=http://d2io1hx8u877l0.cloudfront.net
COL_DIR=2-collectors/scala-stream-collector
COL=snowplow-stream-collector-0.2.0
ENR_DIR=3-enrich/scala-kinesis-enrich
ENR=snowplow-kinesis-enrich-0.2.1

# GeoLiteCity database.
GL=http://geolite.maxmind.com/download/geoip/database/GeoLiteCity.dat.gz

# Snowplow JavaScript tracker.
CF_2=http://d1fc8wv8zag5ca.cloudfront.net/1
SP=sp.js
