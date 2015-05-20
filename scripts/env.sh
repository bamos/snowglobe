# Set JAVA_HOME (and possibly other Java-related variables) if your
# default JDK is not supported by Snowplow.
#export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_05.jdk/Contents/Home

# Separate the variables this much to avoid duplication and
# for easier access in the other scripts.

# Snowplow Scala collector and enricher JARs.
RELEASE_ZIP=http://dl.bintray.com/snowplow/snowplow-generic/snowplow_kinesis_r65_scarlet_rosefinch.zip

# GeoLiteCity database.
GL=http://geolite.maxmind.com/download/geoip/database/GeoLiteCity.dat.gz

# Snowplow JavaScript tracker.
JS_TRACKER=http://d1fc8wv8zag5ca.cloudfront.net/2.4.2/sp.js
