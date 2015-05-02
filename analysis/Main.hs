import Data.Csv(HasHeader(NoHeader))
import Data.Csv(decodeWith,decDelimiter,defaultDecodeOptions)
import Data.Char(ord)

import Data.Time(getCurrentTimeZone)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import SnowGlobe.EnrichedEvent
import SnowGlobe.Time(parse)

main = do
  csvData <- BL.readFile "../data/events.tsv"
  tz <- getCurrentTimeZone
  let v = decodeWith defaultDecodeOptions
          {decDelimiter = fromIntegral $ ord '\t'}
          NoHeader
          csvData :: Either String (V.Vector EnrichedEvent)
  case v of
    Left err -> putStrLn err
    Right v -> V.forM_ v $ \p -> putStrLn $ show $ parse tz $ collectorTstamp p
