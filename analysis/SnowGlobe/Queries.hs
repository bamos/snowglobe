-- Execute analytics queries on lists of EnrichedEvents.
--
-- Brandon Amos <http://bamos.github.io>
-- 2015.05.08

module SnowGlobe.Queries(daySummary, dayReport, weekReport) where

import Data.Function(on)
import Data.List(group, groupBy, intercalate, sort, sortBy)
import Data.Time(LocalTime, TimeZone)
import Data.Geolocation.GeoIP(GeoDB)

import SnowGlobe.EnrichedEvent(EnrichedEvent(..),
                               getLocation, getOrganization)
import SnowGlobe.Time(parse, getTodaysEvents, getWeeksEvents)

-- Summarize today's events so far with the number of total
-- events and visitors.
-- I use this to pipe a summary to my external LCD display,
-- but a concise summary is also useful on desktop-based status bars.
daySummary:: TimeZone -> LocalTime -> [EnrichedEvent] -> String
daySummary tz now events = numEvents ++ " | " ++ numVisitors
    where numEvents = (show . length) todaysEvents
          numVisitors = (show . length . groupByVisitors) todaysEvents
          todaysEvents = getTodaysEvents tz now events

-- Report today's statistics, pages, and visitors.
dayReport:: TimeZone -> LocalTime -> GeoDB -> [EnrichedEvent] -> String
dayReport tz now geo events = formatReport report
    where report = [("Statistics", eventStats todaysEvents),
                    ("Pages", dayPages),
                    ("Referrers", dayReferrers),
                    ("Visitors", intercalate "\n\n" visitorInfo)]
          dayReferrers = sortedEventInfo pageReferrer $ map head visitors
          dayPages = sortedEventInfo pageUrl todaysEvents
          visitorInfo = map (getVisitorInfo geo) sortedVisitors
          sortedVisitors = sortBy (flip compare `on` length) visitors
          visitors = groupByVisitors todaysEvents
          todaysEvents = getTodaysEvents tz now events

-- Summarize today's statistics, pages, and visitors.
weekReport:: TimeZone -> LocalTime -> GeoDB -> [EnrichedEvent] -> String
weekReport tz now geo events = formatReport report
    where report = ("Total", eventStats eventsFlat) :
                   zip dayTitles dayBreakdown
          dayTitles = map fst eventsGrouped
          dayBreakdown = map (eventStats . snd) eventsGrouped
          eventsFlat = concatMap snd eventsGrouped
          eventsGrouped = getWeeksEvents tz now events

-- High-level statistics about a list of events.
eventStats:: [EnrichedEvent] -> String
eventStats events = intercalate "\n"
                    ["+ " ++ (show . length) visitors ++ " unique visitors.",
                     "+ " ++ (show . length) events ++ " total events."]
    where visitors = groupByVisitors events

-- Detailed view of a visitor's interactions.
getVisitorInfo:: GeoDB -> [EnrichedEvent] -> String
getVisitorInfo geo all@(e1:rest) =
    concat $ ["## ", userIpaddress e1, "\n",
            "+ Number of Visits: ", numVisits, "\n",
            "+ OS: ", osName e1, "\n",
            "+ Location: ", getLocation geo e1, "\n",
            "+ Organization: ", getOrganization $ userIpaddress e1, "\n",
            "+ Timezone: ", osTimezone e1, "\n"] ++
            referrerInfo ++
            ["+ Pages (Entry first):\n", pagePath]
    where pagePath = intercalate "\n" $ map (\e -> "  + " ++ pageUrl e) all
          numVisits = show . length $ all
          referrerInfo =
              case pageReferrer e1 of
                "" -> []
                referrer -> ["+ Referrer: ", referrer, "\n"]

-- Reports are represented as a list of (heading, section) tuples.
-- Format them in a Markdown-esque style by marking headings with '#'.
formatReport:: [(String,String)] -> String
formatReport report = intercalate "\n\n" $ map formatSection report
    where formatSection (heading,content) = "# " ++ heading ++ "\n" ++ content

-- Used for per-visitor statistics.
groupByVisitors:: [EnrichedEvent] -> [[EnrichedEvent]]
groupByVisitors events = groupBy ((==) `on` userIpaddress) .
                     sortBy (compare `on` userIpaddress) $
                     events

-- Group, sort, and display a list of events with a field.
-- This is used for computing the top pages and referrers.
--
-- Example output for top pages:
--
--   [NNN Hits]: Page A
--   [MM Hits]: Page B
--   [PP Hits]: Page B
--   [Q Hits]: Page B
sortedEventInfo:: (EnrichedEvent->String) -> [EnrichedEvent] -> String
sortedEventInfo field events =
    intercalate "\n" $ map fieldStr sortedGroupedFields
    where
      fieldStr f = concat ["  [", show $ length f, " hits]: ", head f]
      sortedGroupedFields = sortBy (flip compare `on` length) .
                            group .
                            sort $
                            fields
      fields = map field $ filter (not . null . field) events
