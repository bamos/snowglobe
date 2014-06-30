# SnowGlobe
__Small-scale web analytics for self-hosting and hacking
with Snowplow, Spark, Spray, Twirl, and d3.js.__

SnowGlobe ties together components from the [Snowplow][snowplow]
analytics framework for simple pageview analytics and requires
minimal configuration.
The [JavaScript tracker][js-tracker] is used with the
[Scala collector][scala-collector] and [Scala enrichment][scala-enrichment]
to output [Snowplow canonical output][canonical-output] for the pageviews
as a tab separated file at `events.tsv`.

[Spark][spark] is a Scala data query engine and is used to process
the Snowplow event data.

## Prerequisites, installing, and configuring.
1. Install wget, Python, sbt, scala, and the JRE (&ge; 1.7).
2. Ensure `JAVA_HOME` in `env.sh` contains a Java distribution
   version 1.7 or above.
2. Run `./bootstrap.sh` to download Snowplow and GeoLite binaries.

## Collecting and storing events to TSV files.
On the server, start the collector and enricher with the following
instructions, which can be done in a detached screen or tmux
session, or run by an init daemon.
(TODO: Add upstart script.)

+ `collector.conf` and `enrich.conf` contain sensible default
   configurations for Snowplow's collector and enricher,
   but can be modified as desired.
+ `./start-collect-enrich.sh` will start Snowplow's collector on port
  8080 to pipe Base 64 serialized Thrift raw event output to Snowplow's
  Scala enricher.
  The enricher will output [Snowplow canonical output][canonical-output]
  as rows in `events.tsv`.
+ TODO: Partition events.tsv into a year/month/day directory structure.

## Adding JavaScript tags.
Next, ensure that the collector and enricher are properly configured
and started by running `lynx index.html` on the server.
`events.tsv` should now contain the tab separated event.

Next, copy and paste the following code to your webpage's
templates to send events on the pages you wish to track,
and add Snowplow's JavaScript library `sp.js` to your website's
resource directory.
Make sure to change `localhost:8080` to your server and port,
and ensure the port is open.

```JavaScript
<script src="sp.js" type='text/JavaScript'></script>
<script type="text/JavaScript">
try {
  // Use localhost as the server for testing on the same computer,
  // but change to your deployed server IP address or hostname
  // for production.
  var snowplowTracker = Snowplow.getTrackerUrl('localhost:8080');
  snowplowTracker.enableLinkTracking();
  snowplowTracker.trackPageView();
} catch (err) {}
</script>
```

## Analyzing webpage data with Spark over emails.
TODO

## Analyzing webpage data with Spark, Spray, Twirl, and d3.js.
TODO

## Licensing.

SnowGlobe portions copyright 2014 Brandon Amos under the Apache License,
Version 2.0.
Other portions receive licensing from their parent projects:
Snowplow portions licensed to Snowplow Analytics Ltd.
and Spark portions licensed to the Apache Software Foundation (ASF)
under one or more contributor license agreements.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

[snowplow]: TODO
[js-tracker]: TODO
[scala-enrichment]: TODO
[events-formatting]: TODO
[spark]: TODO
