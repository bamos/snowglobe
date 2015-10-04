-- Time-based helper functions for EnrichedEvents.
--
-- Brandon Amos <http://bamos.github.io>
-- 2015.05.08

module SnowGlobe.Time(parse, getDaysEvents, getWeeksEvents) where

import Control.Applicative
import Data.Time(Day, LocalTime(..), TimeZone, defaultTimeLocale,
                getCurrentTime, getCurrentTimeZone, parseTimeM,
                utcToLocalTime)
import Data.Time.Calendar(addDays, diffDays)

import SnowGlobe.EnrichedEvent

parse :: TimeZone -> String -> Maybe LocalTime
parse tz s = utcToLocalTime tz <$> timeM
             where timeM = parseTimeM True defaultTimeLocale "%F %H:%M:%S%Q" s

isNDaysAgo:: TimeZone -> LocalTime -> Integer -> EnrichedEvent -> Bool
isNDaysAgo tz now n e =
    case eTimeM of
      Nothing -> False
      Just eTime -> (==) diff n
        where diff = diffDays (localDay now) eTime
    where eTimeM = localDay <$> parse tz (collectorTstamp e) :: Maybe Day

getDaysEvents:: TimeZone -> LocalTime -> [EnrichedEvent] -> [EnrichedEvent]
getDaysEvents tz now = filter $ isNDaysAgo tz now 0

getNDaysAgoDate:: LocalTime -> Integer -> String
getNDaysAgoDate now n = show $ addDays (-n) $ localDay now

getWeeksEvents:: TimeZone -> LocalTime -> [EnrichedEvent] ->
                 [(String,[EnrichedEvent])]
getWeeksEvents tz now events = zip days groupedEvents
    where
      days = map (getNDaysAgoDate now) r
      groupedEvents = map (\n -> filter (isNDaysAgo tz now n) events) r
      r = [0..6]
