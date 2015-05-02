module SnowGlobe.Queries(numDailyEvents) where

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
