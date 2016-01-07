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
import SnowGlobe.Time(parse, getDaysEvents, getWeeksEvents)

-- Summarize a day's events so far with the number of total
-- events and visitors.
-- I use this to pipe a summary to my external LCD display,
-- but a concise summary is also useful on desktop-based status bars.
daySummary:: TimeZone -> LocalTime -> [EnrichedEvent] -> String
daySummary tz queryDay events = numEvents ++ " | " ++ numVisitors
    where numEvents = (show . length) daysEvents
          numVisitors = (show . length . groupByVisitors) daysEvents
          daysEvents = getDaysEvents tz queryDay events

-- Report a day's statistics, pages, and visitors.
dayReport:: TimeZone -> LocalTime -> [EnrichedEvent] -> String
dayReport tz queryDay events = formatReport report
    where report = [("Statistics", eventStats daysEvents),
                    ("Pages", dayPages),
                    ("Referrers", dayReferrers),
                    ("Organizations", organizations),
                    ("Locations", locations)]
                    -- ("Visitors", intercalate "\n\n" visitorInfo)]
          dayReferrers = sortedEventInfo referrerStr $ map head visitors
          referrerStr = fromMaybe "" . prettyReferrer
          dayPages = sortedEventInfo pageUrl daysEvents

          organizations = sortedEventInfo orgStr $ map head visitors
          orgStr = getOrganization . userIpaddress

          locations = sortedEventInfo locationStr $ map head visitors
          locationStr e = locationInfo (geoCity e) (geoRegionName e) (geoCountry e)

          -- visitorInfo = map getVisitorInfo sortedVisitors
          -- sortedVisitors = sortBy (flip compare `on` length) visitors

          visitors = groupByVisitors daysEvents
          daysEvents = getDaysEvents tz queryDay events

-- Summarize a week's statistics, pages, and visitors.
weekReport:: TimeZone -> LocalTime -> [EnrichedEvent] -> String
weekReport tz queryDay events = formatReport report
    where report = ("Total", eventStats eventsFlat) :
                   zip dayTitles dayBreakdown
          dayTitles = map fst eventsGrouped
          dayBreakdown = map (eventStats . snd) eventsGrouped
          eventsFlat = concatMap snd eventsGrouped
          eventsGrouped = getWeeksEvents tz queryDay events

-- High-level statistics about a list of events.
eventStats:: [EnrichedEvent] -> String
eventStats events = intercalate "\n"
                    ["+ " ++ (show . length) visitors ++ " unique visitors.",
                     "+ " ++ (show . length) events ++ " total events."]
    where visitors = groupByVisitors events

locationInfo:: String -> String -> String -> String
locationInfo "" "" "" = "Not found"
locationInfo "" "" country = country
locationInfo "" region country = intercalate ", " [region, country]
locationInfo city region country = intercalate ", " [city, region, country]

-- Detailed view of a visitor's interactions.
getVisitorInfo:: [EnrichedEvent] -> String
getVisitorInfo all@(e1:rest) =
    concat $ ["## ", userIpaddress e1, "\n",
              "+ Number of Visits: ", numVisits, "\n",
              "+ OS: ", osName e1, "\n",
              "+ Location: ", locationInfo (geoCity e1) (geoRegionName e1)
                                (geoCountry e1),
              "\n",
              "+ Organization: ", getOrganization $ userIpaddress e1, "\n",
              "+ Timezone: ", osTimezone e1, "\n"] ++
    referrerInfo ++ ["+ Pages (Entry first):\n", pagePath]
    where
      pagePath = intercalate "\n" $ map (\e -> "  + " ++ pageUrl e) all
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
      fieldStr f = concat [ "  ["
                          , show $ length f
                          , if length f == 1 then " hit]: " else " hits]: "
                          , head f ]
      sortedGroupedFields = sortBy (flip compare `on` length) .
                            group .
                            sort $
                            fields
      fields = map field $ filter (not . null . field) events
