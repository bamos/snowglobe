import Data.Csv
import Data.Char(ord)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import SnowGlobe.EnrichedEvent

main = do
  csvData <- BL.readFile "../events.tsv"
  let v = decodeWith defaultDecodeOptions
          {decDelimiter = fromIntegral $ ord '\t'}
          NoHeader
          csvData :: Either String (V.Vector EnrichedEvent)
  case v of
    Left err -> putStrLn err
    Right v -> V.forM_ v $ \p -> putStrLn $ osName p
