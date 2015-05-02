import Data.Char(ord)
import Data.Csv (HasHeader(NoHeader), decodeWith, decDelimiter,
                 defaultDecodeOptions)
import Data.Time(getCurrentTimeZone)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import SnowGlobe.EnrichedEvent
import SnowGlobe.Time(parse)

decodeCsv:: BL.ByteString -> Either String (V.Vector EnrichedEvent)
decodeCsv dat = decodeWith opts NoHeader dat
    where opts = defaultDecodeOptions {decDelimiter = fromIntegral $ ord '\t'}

main = do
  csvData <- BL.readFile "../data/events.tsv"
  tz <- getCurrentTimeZone
  case decodeCsv csvData of
    Left err -> putStrLn err
    Right v -> V.forM_ v $ \p -> print $ parse tz $ collectorTstamp p
