module SnowGlobe.Queries(dailyReport, numDailyEvents) where

import Data.Function(on)
import Data.List(groupBy, intercalate, sortBy)
import Data.Time(Day, LocalTime(..), TimeZone,
                    getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import Data.Geolocation.GeoIP(GeoDB, geoLocateByIPAddress)
import Network.Whois(whois)
import System.IO.Unsafe(unsafePerformIO)
import Text.Regex.Posix

import qualified Data.Geolocation.GeoIP as G
import qualified Data.ByteString.Char8 as B

import SnowGlobe.EnrichedEvent
import SnowGlobe.Time(parse)

isToday:: TimeZone -> LocalTime -> EnrichedEvent -> Bool
isToday tz now e =
    case eTimeM of
      Nothing -> False
      Just eTime -> localDay now == eTime
    where eTimeM = localDay <$> parse tz (collectorTstamp e) :: Maybe Day

getTodaysEvents:: TimeZone -> LocalTime -> [EnrichedEvent] -> [EnrichedEvent]
getTodaysEvents tz now = filter (isToday tz now)

numDailyEvents:: TimeZone -> LocalTime -> [EnrichedEvent] -> Int
numDailyEvents tz now events = length $ getTodaysEvents tz now events

getEventInfo:: (EnrichedEvent->String) -> [EnrichedEvent] -> String
getEventInfo field all@(e1:rest) =
    concat ["  [", show numHits, " ", hits, "]: ", url]
    where numHits = length all
          hits = if numHits == 1 then "Hit" else "Hits"
          url = field e1

topEventInfo:: (EnrichedEvent->String) -> [EnrichedEvent] -> Maybe Int ->
               String
topEventInfo field events mN =
    intercalate "\n" $ map (getEventInfo field) topNFields
    where topNFields = case mN of
                        Nothing -> topFields
                        Just n -> take n topFields
          topFields = sortBy (flip compare `on` length) groupedFields
          groupedFields = groupBy ((==) `on` field) $
                          sortBy (compare `on` field) $
                          filter (not . null . field) events

getGeo:: GeoDB -> EnrichedEvent -> String
getGeo geo event =
    case geoM of
      Nothing -> "Not found"
      Just geo ->
          case (B.unpack $ G.geoCity geo, B.unpack $ G.geoCountryName geo) of
            ("","") -> "Not found"
            ("",country) -> country
            (city, country) -> city ++ ", " ++ country
    where geoM = unsafePerformIO $ geoLocateByIPAddress geo $ B.pack ip
          ip = userIpaddress event

getWhois:: String -> String
getWhois ipAddr =
    case m of
      (Nothing,_) -> "Not found"
      (Just whoisStr,_) ->
          if null r then "Not found" else head r !! 1
          where r = whoisStr =~ "Organization: *(.*)" :: [[String]]
    where m = unsafePerformIO $ whois ipAddr

getVisitorInfo:: GeoDB -> [EnrichedEvent] -> String
getVisitorInfo geo all@(e1:rest) =
    concat ["--- ", userIpaddress e1, " ---\n",
            "+ Number of Visits: ", numVisits, "\n",
            "+ Geo: ", getGeo geo e1, "\n",
            "+ Organization: ", getWhois (userIpaddress e1), "\n",
            "+ Timezone: ", osTimezone e1, "\n",
            "+ Top Pages:\n", topEventInfo pageUrl all Nothing, "\n\n",
            "+ Top Referrers:\n", topEventInfo pageReferrer all Nothing]
    where numVisits = show $ length all

dailyReport:: TimeZone -> LocalTime -> GeoDB -> [EnrichedEvent] -> String
dailyReport tz now geo events = intercalate "\n\n" report
    where report = ["=== Statistics ===", stats,
                    "=== Top " ++ show numTopPages ++ " Pages ===",
                    dailyTopPages,
                    "=== Top " ++ show numTopReferrers ++ " Referrers ===",
                    dailyTopReferrers,
                    "=== Top " ++ show numTopVisitors ++ " Visitors ===",
                    intercalate "\n\n" visitorInfo]
          stats = intercalate "\n"
                  ["+ " ++ show (length visitors) ++ " unique visitors.",
                   "+ " ++ show (length todaysEvents) ++ " total events."]
          dailyTopReferrers = topEventInfo pageReferrer sortedTEvents $ Just numTopReferrers
          dailyTopPages = topEventInfo pageUrl sortedTEvents $ Just numTopPages
          visitorInfo = take numTopVisitors $
                        map (getVisitorInfo geo) sortedVisitors
          sortedVisitors = sortBy (flip compare `on` length) visitors
          visitors = groupBy ((==) `on` userIpaddress) sortedTEvents
          sortedTEvents = sortBy (compare `on` userIpaddress) todaysEvents
          todaysEvents = getTodaysEvents tz now events
          numTopReferrers = 5
          numTopPages = 5
          numTopVisitors = 10
