-- This is the main entry point for SnowGlobe analytics.
-- The command-line options are parsed and functionality is
-- delegated to other portions of the code.
--
-- Brandon Amos <http://bamos.github.io>
-- 2015.05.08

import Data.Char(ord)
import Data.Csv (HasHeader(NoHeader), decodeWith, decDelimiter,
                 defaultDecodeOptions)
import Data.Geolocation.GeoIP(memory_cache, openGeoDB)
import Data.Time(getCurrentTime, getCurrentTimeZone, utcToLocalTime)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import Options.Applicative

import SnowGlobe.EnrichedEvent
import SnowGlobe.Queries(daySummary, dayReport, weekReport)

data Args = Args {events, geoDB, mode :: String} deriving (Show)

-- Argument syntax and help functions.
args :: Parser Args
args = Args <$> strOption (long "events" <> metavar "FILE" <>
                                help "Location of events.tsv" )
       <*> strOption (long "geoDB" <> metavar "FILE" <>
                                help "Location of GeoLiteCity.dat" )
       <*> strOption (long "mode" <> metavar "MODE" <>
                      help "One of: DaySummary, DayReport, WeekReport" )

-- Parse the raw TSV events file into a vector of Snowplow EnrichedEvents.
parseEvents:: BL.ByteString -> Either String (V.Vector EnrichedEvent)
parseEvents = decodeWith opts NoHeader
    where opts = defaultDecodeOptions {decDelimiter = fromIntegral $ ord '\t'}

-- Load data from files and delegate functionality.
analytics:: Args -> IO()
analytics args = do
  rawEvents <- BL.readFile $ events args
  tz <- getCurrentTimeZone
  now <- utcToLocalTime tz <$> getCurrentTime
  geo <- openGeoDB memory_cache $ geoDB args
  case parseEvents rawEvents of
    Left err -> putStrLn err
    Right eventsV ->
        case mode args of
          "DaySummary" -> putStrLn $ daySummary tz now events
          "DayReport" -> putStrLn $ dayReport tz now geo events
          "WeekReport" -> putStrLn $ weekReport tz now geo events
          m -> putStrLn $ "Error: Unexpected mode: " ++ m
        where events = V.toList eventsV

main :: IO ()
main = execParser opts >>= analytics
    where opts = info (helper <*> args)
                 (fullDesc <> header "SnowGlobe Analytics")
