module SnowGlobe.Queries(dailyReport, numDailyEvents) where

import Data.Function(on)
import Data.List(groupBy, intercalate, sortBy)
import Data.Time(Day, LocalTime(..), TimeZone,
                    getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import Data.Geolocation.GeoIP(GeoDB, geoLocateByIPAddress)
import System.IO.Unsafe(unsafePerformIO)

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

getPageInfo:: [EnrichedEvent] -> String
getPageInfo all@(e1:rest) = concat ["  [", show numHits, " ", hits, "]: ", url]
    where numHits = length all
          hits = if numHits == 1 then "Hit" else "Hits"
          url = pageUrl e1

topPageInfo:: [EnrichedEvent] -> Maybe Int -> String
topPageInfo events mN = intercalate "\n" $ map getPageInfo topNPages
    where topNPages = case mN of
                        Nothing -> topPages
                        Just n -> take n topPages
          topPages = sortBy (flip compare `on` length) gPages
          gPages = groupBy ((==) `on` pageUrl) sortedPages
          sortedPages = sortBy (compare `on` pageUrl) events

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

getVisitorInfo:: GeoDB -> [EnrichedEvent] -> String
getVisitorInfo geo all@(e1:rest) = concat
                               ["=== ", userIpaddress e1, " ===\n",
                                "+ Number of Visits: ", numVisits, "\n",
                                "+ Geo: ", getGeo geo e1, "\n",
                                "+ Timezone: ", osTimezone e1, "\n",
                                "+ Top Pages:\n", topPageInfo all Nothing]
    where numVisits = show $ length all

dailyReport:: TimeZone -> LocalTime -> GeoDB -> [EnrichedEvent] -> String
dailyReport tz now geo events = intercalate "\n\n" visitorInfo
    where visitorInfo = map (getVisitorInfo geo) sortedVisitors
          sortedVisitors = sortBy (flip compare `on` length) visitors
          visitors = groupBy ((==) `on` userIpaddress) todaysEvents
          todaysEvents = getTodaysEvents tz now events
