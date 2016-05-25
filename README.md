[![Build Status](https://travis-ci.org/bamos/snowglobe.svg?branch=master)](https://travis-ci.org/bamos/snowglobe)

![](https://raw.githubusercontent.com/bamos/snowglobe/master/images/snowglobe.png)

SnowGlobe provides minimal-configuration web analytics
for small-scale and personal websites.

SnowGlobe integrates components from the
[Snowplow analytics framework][snowplow] to achieve this.
The [JavaScript tracker][js-tracker] pipes into the
[Scala collector][scala-collector] and [Scala enricher][scala-enrichment]
and outputs [Snowplow enriched events][enriched-events]
to a tab separated file.

SnowGlobe uniquely provides Haskell-driven analytics on the data.

**Crafted by [Brandon Amos](http://bamos.github.io).**

# Motivation: Why create SnowGlobe?
I've used [Google Analytics](http://www.google.com/analytics/),
[Piwik](http://piwik.org/), and [Clicky](https://clicky.com)
for my personal website analytics over the last 3 years.
These are great systems and I still use some of them for
the web interfaces and other analytics.
However, I enjoy hacking on the systems I use and
felt limited.

My ongoing goals in building SnowGlobe are to:

1. Pipe a short daily summary of my real-time analytics to my LCD display.
2. Email me daily, weekly, monthly, and yearly reports with
   the information I want.
3. Create interesting real-time visualizations (not started)

# Why not use SQL to query the data?
You should.
I'm using this project to learn Haskell.

## Progress
### Real-time summary on an LCD display
Somebody in my research group gave me a small LCD display
a few weeks ago (in May 2015) and said that he couldn't
find any useful information to put on it,
This, along with my other goals, inspired me to finally
create SnowGlobe.

Piping a daily summary to my LCD display with SnowGlobe works well.
I've replaced my actual stats with the string `NNNN | MMM`, which
represents that I've had `NNNN` total page views by `MMM` visitors.
If you're curious, the other information on the device are the
number of tasks I have (for today, tomorrow, and the next day),
and the temperature and amount of GPU memory I'm currently
using for experiments.

![](https://raw.githubusercontent.com/bamos/snowglobe/master/images/lcd-display.jpg)

### Recurring Reports
Daily recurring reports are also working well.
I plan to expand the information in the weekly reports
and will add monthly and yearly reports in the future.
The following screenshot shows an example report
from my email client, [mutt](http://www.mutt.org/).
More information on setting these reports up below.

![](https://raw.githubusercontent.com/bamos/snowglobe/master/images/sample-report.png)

# Prerequisites, installing, and configuration
1. Install wget, Python, sbt, scala, and the JRE (&ge; 1.7).
2. If the default Java distribution is not version 1.7 or above,
   set `JAVA_HOME` in `scripts/env.sh`.
3. Run `./scripts/bootstrap.sh` to download Snowplow and GeoLite binaries.
4. Update the `uri` in [ip_lookups.json](/conf/enrichments/ip_lookups.json)
   to use the absolute path to your repository.

# Collecting and storing events to TSV files
On the server, start the collector and enricher with the following
instructions.
For persistent collection, use an init daemon (preferred),
or a detached screen or tmux session.

+ `conf/collector.conf` and `conf/enrich.conf` contain sensible default
   configurations for Snowplow's collector and enricher.
+ `scripts/start-collect-enrich.sh` will start Snowplow's collector on port
  8081 to pipe Base 64 serialized Thrift raw event output to Snowplow's
  Scala enricher.
  The enricher will output [Snowplow enriched events][enriched-events]
  as rows in `data/events.tsv`, which will use ~720 bytes per event.
  This script uses `stdbuf` from GNU coreutils to disable stdout
  buffering.
+ [snowglobe.service][snowglobe.service] is a
  [systemctl unit](https://wiki.archlinux.org/index.php/systemd)
  to run `start-collect-enrich.sh`.
  Copy the config with `sudo cp sample/snowglobe.service /etc/systemd/system/`
  and reload the units with `sudo systemctl daemon-reload`.
  Do not use a symlink because of
  [this](https://bugzilla.redhat.com/show_bug.cgi?id=1014311) systemd behavior/bug.
  Start the service with `sudo systemctl start` and
  run on boot with `sudo systemctl enable`.

# Adding JavaScript tags to your webpages
Next, ensure that the collector and enricher are properly configured
and started by opening `sample/index.html` on the server.
`data/events.tsv` should now contain the tab separated event.

Next, copy and paste the following code to your webpage's
templates to send events on the pages you wish to track,
and add Snowplow's JavaScript library `sp.js` to your website's
resource directory.
Make sure to change `localhost:8081` to your server and port,
and ensure the port is open.

```JavaScript
<script src="sp.js" type='text/JavaScript'></script>
<script type="text/JavaScript">
try {
  // Use localhost as the server for testing on the same computer,
  // but change to your deployed server IP address or hostname
  // for production.
  var snowplowTracker = Snowplow.getTrackerUrl('localhost:8081');
  snowplowTracker.enableLinkTracking();
  snowplowTracker.trackPageView();
} catch (err) {}
</script>
```

# Analytics
[Main.hs](https://github.com/bamos/snowglobe/blob/master/analysis/Main.hs)
is the entry point for analytics.
This is a single-threaded application and not ready for large deployments,
but easy to make small changes to.
[optparse-applicative](https://github.com/pcapriotti/optparse-applicative)
parses command-line arguments
and
[cassava](https://github.com/tibbe/cassava)
parses the raw events file into an
`EnrichedEvent`, defined in
[EnrichedEvent.hs](https://github.com/bamos/snowglobe/blob/master/analysis/SnowGlobe/EnrichedEvent.hs),
and contains features of SnowPlow's
[EnrichedEvent.scala](https://github.com/snowplow/snowplow/blob/master/3-enrich/scala-common-enrich/src/main/scala/com.snowplowanalytics.snowplow.enrich/common/outputs/EnrichedEvent.scala).

[EnrichedEvent.hs](https://github.com/bamos/snowglobe/blob/master/analysis/SnowGlobe/EnrichedEvent.hs),
further enriches events by providing
`getLocation` and `getOrganization` functions,
which use
[hs-GeoIP](https://github.com/ozataman/hs-GeoIP)
and
[whois-hs](https://github.com/relrod/whois-hs).
SnowPlow's enricher should add this information to events,
but there's a subtle bug in my configuration
or the software that I'll further track
down in issue
[#4](https://github.com/bamos/snowglobe/issues/4)
sometime.

[Queries.hs](https://github.com/bamos/snowglobe/blob/master/analysis/SnowGlobe/Queries.hs)
groups, filters, and extracts information from the events.

## Building
To build the analytics binary, first install `GHC`, the
Glasgow Haskell Compiler, and `cabal`, a build system
and library database:
  + OSX: `brew install ghc cabal-install`
  + Ubuntu: `apt-get install ghc cabal-install`
  + Arch: `pacman -S ghc cabal-install`

Next, `cd` into the `snowglobe` directory and
install the Haskell dependencies and build
the code with: `cabal install`.
The binary `snowglobe-analytics` is now
in `~/.cabal/bin/snowglobe-analytics`.
You can add `cabal`s bin directory to your `PATH` or use
the full path.

## Command Line Interface

```
$ snowglobe-analytics
SnowGlobe Analytics

Usage: snowglobe-analysis --events FILE COMMAND

Available options:
-h,--help                Show this help text
--events FILE            Location of events.tsv

Available commands:
DaySummary               Print a concise summary for the current day.
DayReport                Print a report for the current day.
WeekReport               Print a report for the past week.
```

## DaySummary: Status on LCD
Using `snowglobe-analytics` in `DaySummary` mode on the command
line outputs the following, showing `NNNN` total page views by `MMM` visitors.

```
$ snowglobe-analysis --events data/events.tsv DaySummary
NNNN | MMM
```

I pipe this output to my LCD display with
[LCD4Linux](https://lcd4linux.bulix.org/)
by defining the following widget in `/etc/lcd4linux.conf`.
LCD4Linux's
[exec documentation](https://lcd4linux.bulix.org/wiki/plugin_exec)
says to use `exec` as a last resort,
but the following is working well for me so far.

```
Widget Hits {
    class 'Text'
    expression exec('su bamos -c "/home/bamos/.cabal/bin/snowglobe-analysis --events /home/bamos/repos/snowglobe/data/events.tsv DaySummary"',10000)
    width 20
    align 'R'
    prefix 'web'
    update 1000
}
```

In the layout, define a row to use the output from this widget.

```
Layout stats {
  ...
  Row2.Col1 'Hits'
  ...
}
```

## Recurring Email Reports
The `DayReport` mode outputs the report in plaintext to stdout.
Pipe this output to your favorite command-line mailer
and add it to your favorite job scheduler.
I use `mutt` and vanilla `cron`, respectively.

The following `cron` entry calls
[analytics-daily-report.sh](https://github.com/bamos/snowglobe/blob/master/scripts/analytics-daily-report.sh) and
[analytics-weekly-report.sh](https://github.com/bamos/snowglobe/blob/master/scripts/analytics-weekly-report.sh).

```
59 23 * * * /home/bamos/repos/snowglobe/scripts/analytics-daily-report.sh
59 23 * * 0 /home/bamos/repos/snowglobe/scripts/analytics-weekly-report.sh
```
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
