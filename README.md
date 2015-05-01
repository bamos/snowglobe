# SnowGlobe

![](https://raw.githubusercontent.com/bamos/snowglobe/master/images/snowglobe.png)

SnowGlobe ties together components from the [Snowplow][snowplow]
analytics framework for simple pageview analytics and requires
minimal configuration.
The [JavaScript tracker][js-tracker] is used with the
[Scala collector][scala-collector] and [Scala enrichment][scala-enrichment]
to output [Snowplow canonical output][canonical-output] for the pageviews
as a tab separated file at `events.tsv`.

# Prerequisites, installing, and configuration
1. Install wget, Python, sbt, scala, and the JRE (&ge; 1.7).
2. Ensure `JAVA_HOME` in `env.sh` contains a Java distribution
   version 1.7 or above.
2. Run `./bootstrap.sh` to download Snowplow and GeoLite binaries.

# Collecting and storing events to TSV files
On the server, start the collector and enricher with the following
instructions, which can be done in a detached screen or tmux
session, or run by an init daemon.

+ `collector.conf` and `enrich.conf` contain sensible default
   configurations for Snowplow's collector and enricher,
   but can be modified as desired.
+ `./start-collect-enrich.sh` will start Snowplow's collector on port
  8080 to pipe Base 64 serialized Thrift raw event output to Snowplow's
  Scala enricher.
  The enricher will output [Snowplow canonical output][canonical-output]
  as rows in `events.tsv`.
  This script uses `stdbuf` from GNU coreutils to disable stdout
  buffering.
+ [systemd.unit]() is a [systemctl unit](https://wiki.archlinux.org/index.php/systemd)
  to run `start-collect-enrich.sh`.
  Copy the config with `sudo cp $PWD/snowglobe.service /etc/systemd/system/`
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
[canonical-output]: https://github.com/snowplow/snowplow/wiki/canonical-event-model
