#!/usr/bin/env ruby

require 'snowplow-tracker'

e = SnowplowTracker::Emitter.new('localhost:8081')
t = SnowplowTracker::Tracker.new(e)
t.track_page_view('http://test.com', "Test page title")
