import Data.Char(ord)
import Data.Csv (HasHeader(NoHeader), decodeWith, decDelimiter,
                 defaultDecodeOptions)
import Data.Time(Day, LocalTime(..), TimeZone,
                    getCurrentTime, getCurrentTimeZone, utcToLocalTime)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import Options.Applicative

import SnowGlobe.EnrichedEvent
import SnowGlobe.Time(parse)

data Args = Args
    { events :: String
    , mode :: String
    } deriving (Show)

args :: Parser Args
args = Args <$> strOption (long "events" <> metavar "FILE" <>
                                help "Location of events.tsv" )
       <*> strOption (long "mode" <> metavar "MODE" <>
                           help "One of: NumDailyEvents" )


decodeCsv:: BL.ByteString -> Either String (V.Vector EnrichedEvent)
decodeCsv = decodeWith opts NoHeader
    where opts = defaultDecodeOptions {decDelimiter = fromIntegral $ ord '\t'}

isToday:: TimeZone -> LocalTime -> EnrichedEvent -> Bool
isToday tz now e =
    case eTimeM of
      Nothing -> False
      Just eTime -> localDay now == eTime
    where eTimeM = fmap localDay $ parse tz $ collectorTstamp e :: Maybe Day

getTodaysEvents:: TimeZone -> LocalTime -> V.Vector EnrichedEvent
        -> V.Vector EnrichedEvent
getTodaysEvents tz now = V.filter (isToday tz now)

dailyEvents:: TimeZone -> LocalTime -> V.Vector EnrichedEvent -> IO()
dailyEvents tz now events = print $ V.length $ getTodaysEvents tz now events

analytics:: Args -> IO()
analytics args = do
  csvData <- BL.readFile $ events args
  tz <- getCurrentTimeZone
  now <- utcToLocalTime tz <$> getCurrentTime
  case decodeCsv csvData of
    Left err -> putStrLn err
    Right events ->
        case mode args of
          "NumDailyEvents" -> dailyEvents tz now events
          m -> putStrLn $ "Error: Unexpected mode: " ++ m

main :: IO ()
main = execParser opts >>= analytics
    where
      opts = info (helper <*> args)
             ( fullDesc <> header "SnowGlobe Analytics" )
