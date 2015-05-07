{-# LANGUAGE ScopedTypeVariables, DeriveGeneric #-}

module SnowGlobe.EnrichedEvent(EnrichedEvent(..), getEventInfo, getGeo,
                              getWhois, sortedEventInfo) where

import Data.Csv
import Data.Function(on)
import Data.Geolocation.GeoIP(GeoDB, geoLocateByIPAddress)
import Data.List(groupBy, intercalate, sortBy)
import GHC.Generics
import Network.Whois(whois)
import System.IO.Unsafe(unsafeDupablePerformIO)
import Text.Regex.Posix

import qualified Data.Geolocation.GeoIP as G
import qualified Data.ByteString.Char8 as B

-- This is a subset of Snowplow's EnrichedEvent object that's compatible
-- with version 0.2.1 of snowplow-kinesis-enrich.
-- Snowplow's EnrichedEvent object can be found at:
-- https://github.com/snowplow/snowplow/blob/master/3-enrich/scala-common-enrich/src/main/scala/com.snowplowanalytics.snowplow.enrich/common/outputs/EnrichedEvent.scala
data EnrichedEvent = EnrichedEvent {
-- The application (site, game, app etc) this event belongs to, and the tracker platform
     appId:: String
    ,platform:: String

-- Date/time
    ,etlTstamp:: String
    ,collectorTstamp:: String
    ,dvceTstamp:: String

-- Transaction (i.e. this logging event)
    ,event:: String
    ,eventId:: String
    ,txnId:: String

-- Versioning
    ,nameTracker:: String
    ,vTracker:: String
    ,vCollector:: String
    ,vEtl:: String

-- User and visit
    ,userId:: String
    ,userIpaddress:: String
    ,userFingerprint:: String
    ,domainUserid:: String
    ,domainSessionidx:: Int
    ,networkUserid:: String

-- Location
    ,geoCountry:: String
    ,geoRegion:: String
    ,geoCity:: String
    ,geoZipcode:: String
    ,geoLatitude:: Maybe Float
    ,geoLongitude:: Maybe Float
    ,geoRegionName:: String

-- Other IP lookups
    ,ipIsp:: String
    ,ipOrganization:: String
    ,ipDomain:: String
    ,ipNetspeed:: String

-- Page
    ,pageUrl:: String
    ,pageTitle:: String
    ,pageReferrer:: String

-- Page URL components
    ,pageUrlscheme:: String
    ,pageUrlhost:: String
    ,pageUrlport:: Maybe Int
    ,pageUrlpath:: String
    ,pageUrlquery:: String
    ,pageUrlfragment:: String

-- Referrer URL components
    ,refrUrlscheme:: String
    ,refrUrlhost:: String
    ,refrUrlport:: Maybe Int
    ,refrUrlpath:: String
    ,refrUrlquery:: String
    ,refrUrlfragment:: String

-- Referrer details
    ,refrMedium:: String
    ,refrSource:: String
    ,refrTerm:: String

-- Marketing
    ,mktMedium:: String
    ,mktSource:: String
    ,mktTerm:: String
    ,mktContent:: String
    ,mktCampaign:: String

-- Custom Contexts
    ,contexts:: String

-- Structured Event
    ,seCategory:: String
    ,seAction:: String
    ,seLabel:: String
    ,seProperty:: String
    ,seValue:: String

-- Unstructured Event
    ,unstructEvent:: String

-- Ecommerce transaction (from querystring)
    ,trOrderid:: String
    ,trAffiliation:: String
    ,trTotal:: String
    ,trTax:: String
    ,trShipping:: String
    ,trCity:: String
    ,trState:: String
    ,trCountry:: String

-- Ecommerce transaction item (from querystring)
    ,tiOrderid:: String
    ,tiSku:: String
    ,tiName:: String
    ,tiCategory:: String
    ,tiPrice:: String
    ,tiQuantity:: String

-- Page Pings
    ,ppXoffsetMin:: Maybe Int
    ,ppXoffsetMax:: Maybe Int
    ,ppYoffsetMin:: Maybe Int
    ,ppYoffsetMax:: Maybe Int

-- User Agent
    ,useragent:: String

-- Browser (from user-agent)
    ,brName:: String
    ,brFamily:: String
    ,brVersion:: String
    ,brType:: String
    ,brRenderengine:: String

-- Browser (from querystring)
    ,brLang:: String

-- Individual feature fields for non-Hive targets (e.g. Infobright)
    ,brFeaturesPdf:: String
    ,brFeaturesFlash:: String
    ,brFeaturesJava:: String
    ,brFeaturesDirector:: String
    ,brFeaturesQuicktime:: String
    ,brFeaturesRealplayer:: String
    ,brFeaturesWindowsmedia:: String
    ,brFeaturesGears:: String
    ,brFeaturesSilverlight:: String
    ,brCookies:: String
    ,brColordepth:: String
    ,brViewwidth:: Maybe Int
    ,brViewheight:: Maybe Int

-- OS (from user-agent)
    ,osName:: String
    ,osFamily:: String
    ,osManufacturer:: String
    ,osTimezone:: String

-- Device/Hardware (from user-agent)
    ,dvceType:: String
    ,dvceIsmobile:: String

-- Device (from querystring)
    ,dvceScreenwidth:: Maybe Int
    ,dvceScreenheight:: Maybe Int

-- Document
    ,docCharset:: String
    ,docWidth:: Maybe Int
    ,docHeight:: Maybe Int
} deriving (Generic,Show)

instance FromRecord EnrichedEvent

getEventInfo:: (EnrichedEvent->String) -> [EnrichedEvent] -> String
getEventInfo field all@(e1:rest) =
    concat ["  [", show numHits, " ", hits, "]: ", url]
    where numHits = length all
          hits = if numHits == 1 then "Hit" else "Hits"
          url = field e1

sortedEventInfo:: (EnrichedEvent->String) -> [EnrichedEvent] -> String
sortedEventInfo field events =
    intercalate "\n" . map (getEventInfo field) $ fields
    where fields = sortBy (flip compare `on` length) groupedFields
          groupedFields = groupBy ((==) `on` field) .
                          sortBy (compare `on` field) .
                          filter (not . null . field) $
                          events

getGeo:: GeoDB -> EnrichedEvent -> String
getGeo geo event =
    case geoM of
      Nothing -> "Not found"
      Just geo ->
          case (B.unpack . G.geoCity $ geo,
                B.unpack . G.geoCountryName $ geo) of
            ("","") -> "Not found"
            ("",country) -> country
            (city, country) -> city ++ ", " ++ country
    where geoM = unsafeDupablePerformIO . geoLocateByIPAddress geo $ ip
          ip = B.pack . userIpaddress $ event

getWhois:: String -> String
getWhois ipAddr =
    case m of
      (Nothing,_) -> "Not found"
      (Just whoisStr,_) ->
          if null r then "Not found" else head r !! 1
          where r = whoisStr =~ "Organization: *(.*)" :: [[String]]
    where m = unsafeDupablePerformIO . whois $ ipAddr
