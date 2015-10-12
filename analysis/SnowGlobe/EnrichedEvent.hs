-- This is a subset of Snowplow's EnrichedEvent object that's compatible
-- with version 0.2.1 of snowplow-kinesis-enrich.
-- Snowplow's EnrichedEvent object can be found at:
-- https://github.com/snowplow/snowplow/blob/master/3-enrich/scala-common-enrich/src/main/scala/com.snowplowanalytics.snowplow.enrich/common/outputs/EnrichedEvent.scala
--
-- This module also provides functions to extract geographic
-- and WHOIS information from the events.
-- This functionality should be in the event objects,but
-- SnowGlobe possibly misconfigures the enricher that causes
-- them to not be added.
--
-- Brandon Amos <http://bamos.github.io>
-- 2015.05.08

{-# LANGUAGE ScopedTypeVariables,DeriveGeneric #-}

module SnowGlobe.EnrichedEvent(EnrichedEvent(..), getOrganization,
                               prettyReferrer) where

import Data.Csv
import Control.Exception(catch, IOException)
import Data.Function(on)
import Data.List(groupBy,intercalate,sortBy)
import GHC.Generics
import Network.Whois(whois)
import System.IO.Unsafe(unsafeDupablePerformIO)
import Text.Regex.Posix

import qualified Data.ByteString.Char8 as B

getOrganization:: String -> String
getOrganization ipAddr =
    case m of
      (Nothing,_) -> failureMsg
      (Just whoisStr,_) ->
          if null r then failureMsg else head r !! 1
          where r = whoisStr =~ "Organization: *(.*)" :: [[String]]
    where m = unsafeDupablePerformIO
              (Control.Exception.catch
               (whois ipAddr)
               (\(_:: IOException) -> return (Nothing, Nothing)))
          failureMsg = "Not found"

prettyReferrer:: EnrichedEvent -> Maybe String
prettyReferrer e
    | pageReferrer e == "" = Nothing
    | otherwise = Just $ f (pageReferrer e) (refrMedium e)
                           (refrSource e) (refrTerm e)
    where
      f url "search" source "" = source ++ ": [Secure Search]"
      f url "search" source term = concat [source, ": ", show term]
      f url "social" source "" = concat [source, ": ", url]
      f url "social" source term = concat [source, ": (TODO) ", url, ":", term]
      f url "unknown" _ _ = url
      f url "internal" _ _ = url
      f url med source term = "TODO: prettyReferrer: " ++
               intercalate " - " [url, med, source, term]

data EnrichedEvent = EnrichedEvent {
    -- The application this event belongs to and the tracker platform
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

    -- Currency
    ,trCurrency:: String
    ,trTotalBase:: String
    ,trTaxBase:: String
    ,trShippingBase:: String
    ,tiCurrency:: String
    ,tiPriceBase:: String
    ,baseCurrency:: String

    -- Geolocation
    ,geoTimezone:: String

    -- Click ID
    ,mktClickid:: String
    ,mktNetwork:: String

    -- ETL tags
    ,etlTags:: String

    -- Time event was sent
    ,dvceSentTstamp:: String

    -- Referer
    ,refrDomainUserid:: String
    ,refrDvceTstamp:: String

    -- Derived contexts
    ,derivedContexts:: String

    -- Session ID
    ,domainSessionid:: String

    -- Derived timestamp
    ,derivedTstamp:: String
} deriving (Generic,Show)

instance FromRecord EnrichedEvent
