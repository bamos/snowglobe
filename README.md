![](https://raw.githubusercontent.com/bamos/snowglobe/master/images/snowglobe.png)

SnowGlobe provides minimal-configuration web analytics
for small-scale and personal websites.

SnowGlobe integrates components from the
[Snowplow analytics framework][snowplow] to achieve this.
The [JavaScript tracker][js-tracker] is used with the
[Scala collector][scala-collector] and [Scala enrichment][scala-enrichment]
to output [Snowplow enriched events][enriched-events]
as a tab separated file.

SnowGlobe uniquely provides Haskell-driven analytics on the data.

# Prerequisites, installing, and configuration
1. Install wget, Python, sbt, scala, and the JRE (&ge; 1.7).
2. If the default Java distribution is not version 1.7 or above,
   set `JAVA_HOME` in `scripts/env.sh`.
2. Run `./scripts/bootstrap.sh` to download Snowplow and GeoLite binaries.

# Collecting and storing events to TSV files
On the server, start the collector and enricher with the following
instructions, which can be done in a detached screen or tmux
session, or run by an init daemon.

+ `conf/collector.conf` and `conf/enrich.conf` contain sensible default
   configurations for Snowplow's collector and enricher,
   but can be modified as desired.
+ `scripts/start-collect-enrich.sh` will start Snowplow's collector on port
  8080 to pipe Base 64 serialized Thrift raw event output to Snowplow's
  Scala enricher.
  The enricher will output [Snowplow enriched events][enriched-events]
  as rows in `data/events.tsv`.
  This script uses `stdbuf` from GNU coreutils to disable stdout
  buffering.
+ [snowglobe.service][snowglobe.service] is a
  [systemctl unit](https://wiki.archlinux.org/index.php/systemd)
  to run `start-collect-enrich.sh`.
  Copy the config with `sudo cp sample/snowglobe.service /etc/systemd/system/`
  and reload the units with `sudo systemctl daemon-reload`.
  The config cannot be symlinked due to
  [this](https://bugzilla.redhat.com/show_bug.cgi?id=1014311) systemd behavior.
  The service can be started immediately with `sudo systemctl start` and
  run on boot with `sudo systemctl enable`.

# Adding JavaScript tags to your webpages.
Next, ensure that the collector and enricher are properly configured
and started by opening `index.html` on the server.
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

# Analysis, Visualization, and Testing
In-progress.
Please contact [Brandon Amos](http://bamos.github.io)
if you're interested in helping.

# Licensing

SnowGlobe portions copyright 2014-2015 Brandon Amos under the
Apache License.

Snowplow portions retain their licenses from Snowplow Analytics Ltd.
and remain unmodified.

The [original SnowGlobe graphic](https://flic.kr/p/7be69Q)
is open-sourced under
[CC-SA 2.0](https://creativecommons.org/licenses/by-sa/2.0/),
and my modifications are under the same license.

[snowplow]: https://github.com/snowplow/snowplow
[js-tracker]: https://github.com/snowplow/snowplow-javascript-tracker
[scala-collector]: https://github.com/snowplow/snowplow/tree/master/2-collectors/scala-stream-collector
[scala-enrichment]: https://github.com/snowplow/snowplow/tree/master/3-enrich/scala-kinesis-enrich
[snowglobe.service]: https://github.com/bamos/snowglobe/blob/master/sample/snowglobe.service
[enriched-events]: https://github.com/snowplow/snowplow/blob/master/3-enrich/scala-common-enrich/src/main/scala/com.snowplowanalytics.snowplow.enrich/common/outputs/EnrichedEvent.scala
