-- Execute analytics queries on lists of EnrichedEvents.
--
-- Brandon Amos <http://bamos.github.io>
-- 2015.05.08

module SnowGlobe.Queries(daySummary, dayReport, weekReport) where

import Data.Function(on)
import Data.List(group, groupBy, intercalate, sort, sortBy)
import Data.Maybe(fromMaybe)
import Data.Time(LocalTime, TimeZone)

import SnowGlobe.EnrichedEvent(EnrichedEvent(..), getOrganization,
                               prettyReferrer)
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
dayReport:: TimeZone -> LocalTime -> [EnrichedEvent] -> String
dayReport tz now events = formatReport report
    where report = [("Statistics", eventStats todaysEvents),
                    ("Pages", dayPages),
                    ("Referrers", dayReferrers),
                    ("Visitors", intercalate "\n\n" visitorInfo)]
          dayReferrers = sortedEventInfo prettyReferrerStr $ map head visitors
          prettyReferrerStr = (fromMaybe "") . prettyReferrer
          dayPages = sortedEventInfo pageUrl todaysEvents
          visitorInfo = map getVisitorInfo sortedVisitors
          sortedVisitors = sortBy (flip compare `on` length) visitors
          visitors = groupByVisitors todaysEvents
          todaysEvents = getTodaysEvents tz now events

-- Summarize today's statistics, pages, and visitors.
weekReport:: TimeZone -> LocalTime -> [EnrichedEvent] -> String
weekReport tz now events = formatReport report
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
getVisitorInfo:: [EnrichedEvent] -> String
getVisitorInfo all@(e1:rest) =
    concat $ ["## ", userIpaddress e1, "\n",
            "+ Number of Visits: ", numVisits, "\n",
            "+ OS: ", osName e1, "\n",

            -- Assume the user's location is the same for all of their events.
            "+ Location: ",
            geoCity e1, ", ", geoRegionName e1, ", ", geoCountry e1, "\n",

            "+ Organization: ", getOrganization $ userIpaddress e1, "\n",
            "+ Timezone: ", osTimezone e1, "\n"] ++
            referrerInfo ++
            ["+ Pages (Entry first):\n", pagePath]
    where pagePath = intercalate "\n" $ map (\e -> "  + " ++ pageUrl e) all
          numVisits = show . length $ all
          referrerInfo =
              case prettyReferrer e1 of
                Nothing -> []
                Just referrer -> ["+ Referrer: ", referrer, "\n"]

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
