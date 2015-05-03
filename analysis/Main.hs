import Data.Char(ord)
import Data.Csv (HasHeader(NoHeader), decodeWith, decDelimiter,
                 defaultDecodeOptions)
import Data.Geolocation.GeoIP(memory_cache, openGeoDB)
import Data.Time(getCurrentTime, getCurrentTimeZone, utcToLocalTime)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import Options.Applicative

import SnowGlobe.EnrichedEvent
import SnowGlobe.Queries(dayNumEvents, dayReport, weekReport)

data Args = Args {events, geoDB, mode :: String} deriving (Show)

args :: Parser Args
args = Args <$> strOption (long "events" <> metavar "FILE" <>
                                help "Location of events.tsv" )
       <*> strOption (long "geoDB" <> metavar "FILE" <>
                                help "Location of GeoLiteCity.dat" )
       <*> strOption (long "mode" <> metavar "MODE" <>
                      help "One of: DayNumEvents, DayReport, WeekReport" )

decodeCsv:: BL.ByteString -> Either String (V.Vector EnrichedEvent)
decodeCsv = decodeWith opts NoHeader
    where opts = defaultDecodeOptions {decDelimiter = fromIntegral $ ord '\t'}

analytics:: Args -> IO()
analytics args = do
  csvData <- BL.readFile $ events args
  tz <- getCurrentTimeZone
  now <- utcToLocalTime tz <$> getCurrentTime
  geo <- openGeoDB memory_cache $ geoDB args
  case decodeCsv csvData of
    Left err -> putStrLn err
    Right eventsV ->
        case mode args of
          "DayNumEvents" -> print $ dayNumEvents tz now events
          "DayReport" -> putStrLn $ dayReport tz now geo events
          "WeekReport" -> putStrLn $ weekReport tz now geo events
          m -> putStrLn $ "Error: Unexpected mode: " ++ m
        where events = V.toList eventsV

main :: IO ()
main = execParser opts >>= analytics
    where opts = info (helper <*> args)
                 (fullDesc <> header "SnowGlobe Analytics")
