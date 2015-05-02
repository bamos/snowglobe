import Data.Char(ord)
import Data.Csv (HasHeader(NoHeader), decodeWith, decDelimiter,
                 defaultDecodeOptions)
import Data.Time(Day, LocalTime(..), TimeZone,
                    getCurrentTime, getCurrentTimeZone, utcToLocalTime)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import SnowGlobe.EnrichedEvent
import SnowGlobe.Time(parse)

decodeCsv:: BL.ByteString -> Either String (V.Vector EnrichedEvent)
decodeCsv dat = decodeWith opts NoHeader dat
    where opts = defaultDecodeOptions {decDelimiter = fromIntegral $ ord '\t'}

isToday:: TimeZone -> LocalTime -> EnrichedEvent -> Bool
isToday tz now e =
    case eTimeM of
      Nothing -> False
      Just eTime -> localDay now == eTime
    where eTimeM = fmap (localDay) $ parse tz $ collectorTstamp e :: Maybe Day

getTodaysEvents:: TimeZone -> LocalTime -> V.Vector EnrichedEvent
        -> V.Vector EnrichedEvent
getTodaysEvents tz now events = V.filter (isToday tz now) events

main = do
  csvData <- BL.readFile "../data/events.tsv"
  tz <- getCurrentTimeZone
  now <- utcToLocalTime tz <$> getCurrentTime
  case decodeCsv csvData of
    Left err -> putStrLn err
    Right events -> print $ V.length $ getTodaysEvents tz now events
