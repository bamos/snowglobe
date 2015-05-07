module SnowGlobe.Queries(dayNumEvents, dayReport, weekReport) where

import Data.Function(on)
import Data.List(groupBy, intercalate, sortBy)
import Data.Time(LocalTime, TimeZone)
import Data.Geolocation.GeoIP(GeoDB)

import SnowGlobe.EnrichedEvent(EnrichedEvent(..), getEventInfo,
                               getGeo, getWhois, sortedEventInfo)
import SnowGlobe.Time(parse, getTodaysEvents, getWeeksEvents)

dayNumEvents:: TimeZone -> LocalTime -> [EnrichedEvent] -> Int
dayNumEvents tz now = length . getTodaysEvents tz now

getVisitorInfo:: GeoDB -> [EnrichedEvent] -> String
getVisitorInfo geo all@(e1:rest) =
    concat $ ["## ", userIpaddress e1, "\n",
            "+ Number of Visits: ", numVisits, "\n",
            "+ Geo: ", getGeo geo e1, "\n",
            "+ Organization: ", getWhois $ userIpaddress e1, "\n",
            "+ Timezone: ", osTimezone e1, "\n",
            "+ Entry Page:\n", pageUrl e1, "\n",
            "+ Pages:\n", sortedEventInfo pageUrl all, "\n"] ++
            referrerInfo
    where numVisits = show . length $ all
          referrers = sortedEventInfo pageReferrer all
          referrerInfo = if null referrers then []
                         else ["\n+ Referrers:\n", referrers]

getStats:: [EnrichedEvent] -> String
getStats events = intercalate "\n"
                  ["+ " ++ (show . length) visitors ++ " unique visitors.",
                   "+ " ++ (show . length) events ++ " total events."]
    where visitors = groupBy ((==) `on` userIpaddress) .
                     sortBy (compare `on` userIpaddress) $
                     events

formatSection:: (String,String) -> String
formatSection (heading,content) = "# " ++ heading ++ "\n" ++ content

formatReport:: [(String,String)] -> String
formatReport report = intercalate "\n\n" $ map formatSection report

dayReport:: TimeZone -> LocalTime -> GeoDB -> [EnrichedEvent] -> String
dayReport tz now geo events = formatReport report
    where report = [("Statistics", getStats todaysEvents),
                    ("Pages", dayPages),
                    ("Referrers", dayReferrers),
                    ("Visitors", intercalate "\n\n" visitorInfo)]
          dayReferrers = sortedEventInfo pageReferrer todaysEvents
          dayPages = sortedEventInfo pageUrl todaysEvents
          visitorInfo = map (getVisitorInfo geo) sortedVisitors
          sortedVisitors = sortBy (flip compare `on` length) visitors
          visitors = groupBy ((==) `on` userIpaddress) .
                     sortBy (compare `on` userIpaddress) $
                     todaysEvents
          todaysEvents = getTodaysEvents tz now events

weekReport:: TimeZone -> LocalTime -> GeoDB -> [EnrichedEvent] -> String
weekReport tz now geo events = formatReport report
    where report = ("Total", getStats eventsFlat) : zip dayTitles dayBreakdown
          dayTitles = map fst eventsGrouped
          dayBreakdown = map (getStats . snd) eventsGrouped
          eventsFlat = concatMap snd eventsGrouped
          eventsGrouped = getWeeksEvents tz now events
