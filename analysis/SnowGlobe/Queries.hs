module SnowGlobe.Queries(dailyReport, numDailyEvents) where

import Data.Function(on)
import Data.List(groupBy, intercalate, sortBy)
import Data.Time(Day, LocalTime(..), TimeZone,
                    getCurrentTime, getCurrentTimeZone, utcToLocalTime)

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
topPageInfo events mN = intercalate "\n" $ map getPageInfo $ topNPages
    where topNPages = case mN of
                        Nothing -> topPages
                        Just n -> take n $ topPages
          topPages = reverse . sortBy (compare `on` length) $ gPages
          gPages = groupBy ((==) `on` pageUrl) $ sortedPages
          sortedPages = sortBy (compare `on` pageUrl) events

getVisitorInfo:: [EnrichedEvent] -> String
getVisitorInfo all@(e1:rest) = concat ["=== ", userIpaddress e1, " ===\n",
                               "+ Number of Visits: ", numVisits, "\n",
                               "+ Timezone: ", osTimezone e1, "\n",
                               "+ Top Pages:\n", topPageInfo all Nothing]
                               where numVisits = show $ length all

dailyReport:: TimeZone -> LocalTime -> [EnrichedEvent] -> String
dailyReport tz now events = intercalate "\n\n" visitorInfo
    where visitorInfo = map getVisitorInfo $ sortedVisitors
          sortedVisitors = reverse . sortBy (compare `on` length) $ visitors
          visitors = groupBy ((==) `on` userIpaddress) $ todaysEvents
          todaysEvents = getTodaysEvents tz now events
