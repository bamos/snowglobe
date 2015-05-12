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
import Data.List(isInfixOf)
import Data.Time(getCurrentTime, getCurrentTimeZone, utcToLocalTime)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import Options.Applicative

import SnowGlobe.EnrichedEvent
import SnowGlobe.Queries(daySummary, dayReport, weekReport)

data Args = Args
    { events, geoDB :: String
    , mode :: Mode
    } deriving Show

-- Argument syntax and help functions.
args :: Parser Args
args = Args
       <$> strOption (long "events" <> metavar "FILE" <>
                      help "Location of events.tsv" )
       <*> strOption (long "geoDB" <> metavar "FILE" <>
                      help "Location of GeoLiteCity.dat" )
       <*> modeParser

data Mode
    = DaySummary
    | DayReport
    | WeekReport
    deriving Show

modeParser :: Parser Mode
modeParser = subparser
             (  command "DaySummary" dsInfo
             <> command "DayReport" drInfo
             <> command "WeekReport" wrInfo
             )
    where
      mInfo :: Mode -> String -> ParserInfo Mode
      mInfo m d = info (pure m) (progDesc d)
      dsInfo = mInfo DaySummary "Print a concise summary for the current day."
      drInfo = mInfo DayReport "Print a report for the current day."
      wrInfo = mInfo WeekReport "Print a report for the past week."

-- Parse the raw TSV events file into a vector of Snowplow EnrichedEvents.
parseEvents:: BL.ByteString -> Either String (V.Vector EnrichedEvent)
parseEvents = decodeWith opts NoHeader
    where opts = defaultDecodeOptions {decDelimiter = fromIntegral $ ord '\t'}

-- Load data from files and delegate functionality.
run:: Args -> IO()
run args = do
  rawEvents <- BL.readFile $ events args
  tz <- getCurrentTimeZone
  now <- utcToLocalTime tz <$> getCurrentTime
  geo <- openGeoDB memory_cache $ geoDB args
  case parseEvents rawEvents of
    Left err -> putStrLn err
    Right eventsV ->
        case mode args of
          DaySummary -> putStrLn $ daySummary tz now events
          DayReport -> putStrLn $ dayReport tz now geo events
          WeekReport -> putStrLn $ weekReport tz now geo events
        where events = filter isMine $ V.toList eventsV
              isMine e = any (\domain -> isInfixOf domain $ pageUrl e) whitelist
              whitelist = ["bamos.github.io", "derecho.elijah"]

main :: IO ()
main = execParser opts >>= run
    where opts = info (helper <*> args)
                 (fullDesc <> header "SnowGlobe Analytics")
