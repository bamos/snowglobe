{-# LANGUAGE ScopedTypeVariables, DeriveGeneric #-}

module SnowGlobe.EnrichedEvent where

import Data.Csv
import GHC.Generics

-- Event type for version 0.2.1 of snowplow-kinesis-enrich.
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
