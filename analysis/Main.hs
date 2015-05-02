import Data.Char(ord)
import Data.Csv (HasHeader(NoHeader), decodeWith, decDelimiter,
                 defaultDecodeOptions)
import Data.Time(getCurrentTime, getCurrentTimeZone, utcToLocalTime)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import Options.Applicative

import SnowGlobe.EnrichedEvent
import SnowGlobe.Queries(dailyReport, numDailyEvents)

data Args = Args {events :: String , mode :: String} deriving (Show)

args :: Parser Args
args = Args <$> strOption (long "events" <> metavar "FILE" <>
                                help "Location of events.tsv" )
       <*> strOption (long "mode" <> metavar "MODE" <>
                           help "One of: NumDailyEvents, DailyReport" )

decodeCsv:: BL.ByteString -> Either String (V.Vector EnrichedEvent)
decodeCsv = decodeWith opts NoHeader
    where opts = defaultDecodeOptions {decDelimiter = fromIntegral $ ord '\t'}

analytics:: Args -> IO()
analytics args = do
  csvData <- BL.readFile $ events args
  tz <- getCurrentTimeZone
  now <- utcToLocalTime tz <$> getCurrentTime
  case decodeCsv csvData of
    Left err -> putStrLn err
    Right eventsV ->
        case mode args of
          "NumDailyEvents" -> print $ numDailyEvents tz now events
          "DailyReport" -> putStrLn $ dailyReport tz now events
          m -> putStrLn $ "Error: Unexpected mode: " ++ m
        where events = V.toList eventsV

main :: IO ()
main = execParser opts >>= analytics
    where opts = info (helper <*> args)
                 (fullDesc <> header "SnowGlobe Analytics")
