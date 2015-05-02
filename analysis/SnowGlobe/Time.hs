module SnowGlobe.Time(parse) where

import Data.Time(LocalTime, TimeZone, defaultTimeLocale, parseTimeM,
                 utcToLocalTime)

parse :: TimeZone -> String -> Maybe LocalTime
parse tz s = utcToLocalTime tz <$> timeM
             where timeM = parseTimeM True defaultTimeLocale "%F %H:%M:%S%Q" s
