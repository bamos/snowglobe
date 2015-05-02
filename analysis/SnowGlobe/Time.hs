module SnowGlobe.Time(parse) where

import Data.Time(LocalTime,TimeZone)
import Data.Time(defaultTimeLocale,parseTimeM,utcToLocalTime)

parse :: TimeZone -> String -> Maybe LocalTime
parse tz s = utcToLocalTime tz <$> timeM
             where timeM = parseTimeM True defaultTimeLocale "%F %H:%M:%S%Q" s
