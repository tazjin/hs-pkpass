{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}

{- |This module provides types and functions for type-safe generation of PassBook's @pass.json@ files.

    This is a complete implementation of the Passbook Package Format Reference, available at
    <https://developer.apple.com/library/ios/#documentation/UserExperience/Reference/PassKit_Bundle/Chapters/Introduction.html>.


    It ensures that passes are created correctly wherever possible. Currently, NSBundle localization is not supported.
-}
module Passbook.Types(
    -- * Passbook field types
      PassValue(..)
    , RelevantDate
    , Location(..)
    , RGBColor
    , BarcodeFormat(..)
    , Barcode(..)
    , Alignment(..)
    , DateTimeStyle(..)
    , NumberStyle(..)
    , TransitType(..)
    , WebService(..)
    -- * Passbook types
    , PassField(..)
    , PassType(..)
    , PassContent(..)
    , Pass(..)
    -- * Auxiliary functions
    , rgb
    , mkBarcode
    , mkSimpleField
    ) where

import           Control.Applicative    (pure, (<$>), (<*>))
import           Control.Monad          (mzero)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types       hiding (Parser)
import           Data.Attoparsec.Number
import           Data.Attoparsec.Text
import qualified Data.HashMap.Strict    as HM
import           Data.Text              (Text, pack, unpack)
import           Data.Time
import           Data.Typeable
import           System.Locale
import           Text.Shakespeare.Text

-- | Auxiliary type to ensure that field values are rendered correctly
data PassValue = PassInt Integer
               | PassDouble Double
               | PassDate UTCTime
               | PassText Text
    deriving (Eq, Ord, Show, Typeable)

-- * Passbook data types

-- |Newtype wrapper around 'UTCTime' for use in the @relevantDate@ field of a pass.
newtype RelevantDate = RelevantDate UTCTime deriving (Eq, Ord, Show, Typeable)

-- |A location field
data Location = Location {
      latitude     :: Double -- ^ Latitude, in degrees, of the location (required)
    , longitude    :: Double -- ^ Longitude, in degrees, of the location (required)
    , altitude     :: Maybe Double -- ^ Altitude, in meters, of the location (optional)
    , relevantText :: Maybe Text -- ^ Text displayed on the lock screen when the pass is relevant (optional)
} deriving (Eq, Ord, Show, Typeable)

-- |A simple RGB color value. In combination with the 'rgb' function this can be written just like in
--  CSS, e.g. @rgb(43, 53, 65)@. The 'rgb' function also ensures that the provided values are valid.
data RGBColor = RGB Int Int Int
    deriving (Eq, Ord, Show, Typeable)

-- |Barcode is constructed by a Barcode format, an encoding
--  type and the Barcode message.
data BarcodeFormat = QRCode
                   | PDF417
                   | Aztec
    deriving (Eq, Ord, Show, Typeable)

-- |A pass barcode. In most cases the helper function 'mkBarcode' should be sufficient.
data Barcode = Barcode {
      altText         :: Maybe Text -- ^ Text displayed near the barcode (optional)
    , format          :: BarcodeFormat -- ^ Barcode format (required)
    , message         :: Text -- ^ Message / payload to be displayed as a barcode (required)
    , messageEncoding :: Text -- ^ Barcode encoding. Default in the mkBarcode functions is iso-8859-1 (required)
} deriving (Eq, Ord, Show, Typeable)

-- |Pass field alignment
data Alignment = LeftAlign
               | Center
               | RightAlign
               | Natural
    deriving (Eq, Ord, Typeable)

-- |Pass field date/time display style
data DateTimeStyle = None -- ^ Corresponds to @NSDateFormatterNoStyle@
                   | Short -- ^ Corresponds to @NSDateFormatterShortStyle@
                   | Medium -- ^ Corresponds to @NSDateFormatterMediumStyle@
                   | Long -- ^ Corresponds to @NSDateFormatterLongStyle@
                   | Full -- ^ Corresponds to @NSDateFormatterFullStyle@
    deriving (Eq, Ord, Typeable)

-- |Pass field number display style
data NumberStyle = Decimal
                 | Percent
                 | Scientific
                 | SpellOut
    deriving (Eq, Ord, Typeable)

-- |A single pass field. The type 'PassValue' holds the fields value and ensures that the JSON output is compatible with Passbook.
--  To create a very simple key/value field containing text you can use the 'mkSimpleField' function.
data PassField = PassField {
    -- standard field keys
      changeMessage :: Maybe Text -- ^ Message displayed when the pass is updated. May contain the @%\@@ placeholder for the value. (optional)
    , key           :: Text -- ^ Must be a unique key within the scope of the pass (e.g. \"departure-gate\") (required)
    , label         :: Maybe Text -- ^ Label text for the field. (optional)
    , textAlignment :: Maybe Alignment -- ^ Alignment for the field's contents. Not allowed for primary fields. (optional)
    , value         :: PassValue -- ^ Value of the field. Must be a string, ISO 8601 date or a number. (required)

    -- Date style keys (all optional). If any key is present, the field will be treated as a date.
    , dateStyle     :: Maybe DateTimeStyle -- ^ Style of date to display (optional)
    , timeStyle     :: Maybe DateTimeStyle -- ^ Style of time to display (optional)
    , isRelative    :: Maybe Bool -- ^ Is the date/time displayed relative to the current time or absolute? Default: @False@ (optional)

    -- Number style keys (all optional). Not allowed if the field is not a number.
    , currencyCode  :: Maybe Text -- ^ ISO 4217 currency code for the field's value (optional)
    , numberStyle   :: Maybe NumberStyle -- ^ Style of number to display. See @NSNumberFormatterStyle@ docs for more information. (optional)
} deriving (Eq, Ord, Typeable)

-- |BoardingPass transit type. Only necessary for Boarding Passes.
data TransitType = Air
                 | Boat
                 | Bus
                 | Train
                 | GenericTransit
    deriving (Eq, Ord, Typeable)

-- |The type of a pass including its fields
data PassType = BoardingPass TransitType PassContent
              | Coupon PassContent
              | Event PassContent
              | GenericPass PassContent
              | StoreCard PassContent
    deriving (Eq, Ord, Typeable)

data WebService = WebService {
      authenticationToken :: Text -- ^ Authentication token for use with the web service. Must be 16 characters or longer (optional)
    , webServiceURL       :: Text -- ^ The URL of a web service that conforms to the API described in the Passbook Web Service Reference (optional)
} deriving (Eq, Ord, Show, Typeable)

-- |The fields within a pass
data PassContent = PassContent {
      headerFields    :: [PassField] -- ^ Fields to be displayed on the front of the pass. Always shown in the stack.
    , primaryFields   :: [PassField] -- ^ Fields to be displayed prominently on the front of the pass.
    , secondaryFields :: [PassField] -- ^ Fields to be displayed on the front of the pass.
    , auxiliaryFields :: [PassField] -- ^ Additional fields to be displayed on the front of the pass.
    , backFields      :: [PassField] -- ^ Fields to be on the back of the pass.
} deriving (Eq, Ord, Typeable)

-- |A complete pass
data Pass = Pass {
    -- Required keys
      description                :: Text -- ^ Brief description of the pass (required)
    , organizationName           :: Text -- ^ Display name of the organization that signed the pass (required)
    , passTypeIdentifier         :: Text -- ^ Pass type identifier, as issued by Apple (required)
    , serialNumber               :: Text -- ^ Unique serial number for the pass (required)
    , teamIdentifier             :: Text -- ^ Team identifier for the organization (required)

    -- associated app keys
    , associatedStoreIdentifiers :: [Text] -- ^ A list of iTunes Store item identifiers for associated apps (optional)

    -- relevance keys
    , locations                  :: [Location]  -- ^ Locations where the pass is relevant (e.g. that of a store) (optional)
    , relevantDate               :: Maybe RelevantDate -- ^ ISO 8601 formatted date for when the pass becomes relevant (optional)

    -- visual appearance key
    , barcode                    :: Maybe Barcode -- ^ Barcode information (optional)
    , backgroundColor            :: Maybe RGBColor -- ^ Background color of the pass (optional)
    , foregroundColor            :: Maybe RGBColor -- ^ Foreground color of the pass (optional)
    , labelColor                 :: Maybe Text -- ^ Color of the label text. If omitted, the color is determined automatically. (optional)
    , logoText                   :: Maybe Text -- ^ Text displayed next to the logo on the pass (optional)
    , suppressStripShine         :: Maybe Bool -- ^ If @True@, the strip image is displayed without a shine effect. (optional)

    -- web service keys
    , webService                 :: Maybe WebService -- ^ Contains the authentication token (16 characters or longer) and the API end point for a Web Service

    , passContent                :: PassType -- ^ The kind of pass and the passes' fields (required)
} deriving (Eq, Ord, Typeable)

-- * JSON instances

-- |Conditionally appends something wrapped in Maybe to a list of 'Pair'. This is necessary
--  because Passbook can't deal with null values in JSON.
(-:) :: ToJSON a => Text -> Maybe a -> ([Pair] -> [Pair])
(-:) _ Nothing = id
(-:) key (Just value) = ((key .= value) :)

$(deriveToJSON id ''PassContent)

instance ToJSON Location where
    toJSON Location{..} =
      let pairs =   ("altitude" -: altitude)
                  $ ("relevantText" -: relevantText)
                  $ ["latitude" .= latitude
                    ,"longitude" .= longitude]
      in object pairs

instance ToJSON Barcode where
  toJSON Barcode{..} =
    let pairs =   ("altText" -: altText)
                $ [ "format" .= format
                  , "message" .= message
                  , "messageEncoding" .= messageEncoding ]
    in object pairs

instance ToJSON PassField where
    toJSON PassField{..} =
      let pairs =   ("changeMessage" -: changeMessage)
                  $ ("label" -: label)
                  $ ("textAlignment" -: textAlignment)
                  $ ("dateStyle" -: dateStyle)
                  $ ("timeStyle" -: timeStyle)
                  $ ("isRelative" -: isRelative)
                  $ ("currencyCode" -: currencyCode)
                  $ ("numberStyle" -: numberStyle)
                  $ ["key" .= key, "value" .= value]
      in object pairs


instance ToJSON Pass where
    toJSON Pass{..} =
      let pairs =   ("relevantDate" -: relevantDate)
                  $ ("barcode" -: barcode)
                  $ ("backgroundColor" -: backgroundColor)
                  $ ("foregroundColor" -: foregroundColor)
                  $ ("labelColor" -: labelColor)
                  $ ("logoText" -: logoText)
                  $ ("suppressStripShine" -: suppressStripShine)
                  $ ("authenticationToken" -: (fmap authenticationToken) webService)
                  $ ("webServiceURL" -: (fmap webServiceURL) webService)
                  $ [ "description" .= description
                    , "formatVersion" .= (1 :: Int) -- Hardcoding this because it should not be changed
                    , "organizationName" .= organizationName
                    , "passTypeIdentifier" .= passTypeIdentifier
                    , "serialNumber" .= serialNumber
                    , "teamIdentifier" .= teamIdentifier
                    , "associatedStoreIdentifiers" .= associatedStoreIdentifiers
                    , "locations" .= locations
                    , (pack $ show passContent) .= passContent]
      in object pairs

-- |Internal helper function to handle Boarding Passes correctly.
getPassContent :: PassType -> PassContent
getPassContent pc = case pc of
    BoardingPass _ pc -> pc
    Coupon pc         -> pc
    Event pc          -> pc
    GenericPass pc    -> pc
    StoreCard pc      -> pc

instance ToJSON PassType where
    toJSON (BoardingPass tt PassContent{..}) = object [
        "transitType" .= tt
      , "headerFields" .= headerFields
      , "primaryFields" .= primaryFields
      , "secondaryFields" .= secondaryFields
      , "auxiliaryFields" .= auxiliaryFields
      , "backFields" .= backFields ]
    toJSON pt = toJSON $ getPassContent pt

-- |Internal helper function
renderRGB :: RGBColor -> Text
renderRGB (RGB r g b) = [st|rgb(#{show r},#{show g},#{show b})|]

instance ToJSON RGBColor where
    toJSON = toJSON . renderRGB

instance ToJSON BarcodeFormat where
    toJSON QRCode = toJSON ("PKBarcodeFormatQR" :: Text)
    toJSON PDF417 = toJSON ("PKBarcodeFormatPDF417" :: Text)
    toJSON Aztec  = toJSON ("PKBarcodeFormatAztec" :: Text)

instance Show Alignment where
    show LeftAlign = "PKTextAlignmentLeft"
    show Center = "PKTextAlignmentCenter"
    show RightAlign = "PKTextAlignmentRight"
    show Natural = "PKTextAlignment"

instance ToJSON Alignment where
    toJSON = toJSON . pack . show

instance Show DateTimeStyle where
    show None = "NSDateFormatterNoStyle"
    show Short = "NSDateFormatterShortStyle"
    show Medium = "NSDateFormatterMediumStyle"
    show Long = "NSDateFormatterLongStyle"
    show Full = "NSDateFormatterFullStyle"

instance ToJSON DateTimeStyle where
    toJSON = toJSON . pack . show

instance Show NumberStyle where
    show Decimal = "PKNumberStyleDecimal"
    show Percent = "PKNumberStylePercent"
    show Scientific = "PKNumberStyleScientific"
    show SpellOut = "PKNumberStyleSpellOut"

instance ToJSON NumberStyle where
    toJSON = toJSON . pack . show

instance Show TransitType where
    show Air = "PKTransitTypeAir"
    show Boat = "PKTransitTypeBoat"
    show Bus = "PKTransitTypeBus"
    show Train = "PKTransitTypeTrain"
    show GenericTransit = "PKTransitTypeGeneric"

instance ToJSON TransitType where
    toJSON = toJSON . pack . show

instance ToJSON PassValue where
    toJSON (PassInt i) = toJSON i
    toJSON (PassDouble d) = toJSON d
    toJSON (PassText t) = toJSON t
    toJSON (PassDate d) = jsonPassdate d

instance ToJSON RelevantDate where
    toJSON (RelevantDate d) = jsonPassdate d

-- | The ISO 8601 time/date encoding used by Passbook
timeFormat = iso8601DateFormat $ Just $ timeFmt defaultTimeLocale

-- |Correctly renders a @PassDate@ in JSON (ISO 8601)
jsonPassdate = toJSON . formatTime defaultTimeLocale timeFormat

-- |Helper function that parses a 'UTCTime' out of a Text
parseJsonDate :: Text -> Maybe UTCTime
parseJsonDate = parseTime defaultTimeLocale timeFormat . unpack

instance Show PassType where
    show (BoardingPass _ _) = "boardingPass"
    show (Coupon _) = "coupon"
    show (Event _) = "eventTicket"
    show (GenericPass _) = "generic"
    show (StoreCard _) = "storeCard"

deriving instance Show PassField
deriving instance Show PassContent
deriving instance Show Pass

-- * Implementing FromJSON

instance FromJSON Alignment where
    parseJSON (String t) = case t of
        "PKTextAlignmentLeft" -> pure LeftAlign
        "PKTextAlignmentCenter" -> pure Center
        "PKTextAlignmentRight" -> pure RightAlign
        "PKTextAlignment" -> pure Natural
        _ -> fail "Could not parse text alignment style"
    parseJSON _ = mzero

instance FromJSON DateTimeStyle where
    parseJSON (String t) = case t of
        "NSDateFormatterNoStyle" -> pure None
        "NSDateFormatterShortStyle" -> pure Short
        "NSDateFormatterMediumStyle" -> pure Medium
        "NSDateFormatterLongStyle" -> pure Long
        "NSDateFormatterFullStyle" -> pure Full
        _ -> fail "Could not parse date formatting style"
    parseJSON _ = mzero

instance FromJSON NumberStyle where
    parseJSON (String t) = case t of
        "PKNumberStyleDecimal" -> pure Decimal
        "PKNumberStylePercent" -> pure Percent
        "PKNumberStyleScientific" -> pure Scientific
        "PKNumberStyleSpellOut" -> pure SpellOut
        _ -> fail "Could not parse number formatting style"
    parseJSON _ = mzero

instance FromJSON BarcodeFormat where
    parseJSON (String t) = case t of
        "PKBarcodeFormatQR" -> pure QRCode
        "PKBarcodeFormatAztec" -> pure Aztec
        "PKBarcodeFormatPDF417" -> pure PDF417
        _ -> fail "Could not parse barcode format"
    parseJSON _ = mzero

instance FromJSON TransitType where
    parseJSON (String t) = case t of
        "PKTransitTypeAir" -> pure Air
        "PKTransitTypeBoat" -> pure Boat
        "PKTransitTypeBus" -> pure Bus
        "PKTransitTypeTrain" -> pure Train
        "PKTransitTypeGeneric" -> pure GenericTransit
        _ -> fail "Could not parse transit type"
    parseJSON _ = mzero

instance FromJSON Location where
    parseJSON (Object v) = Location         <$>
                           v .: "latitude"  <*>
                           v .: "longitude" <*>
                           v .:? "altitude" <*>
                           v .:? "relevantText"
    parseJSON _ = mzero

instance FromJSON Barcode where
    parseJSON (Object v) = Barcode         <$>
                           v .:? "altText" <*>
                           v .: "format"   <*>
                           v .: "message"  <*>
                           v .: "messageEncoding"
    parseJSON _ = mzero

instance FromJSON PassValue where
    parseJSON (Number (I i)) = pure $ PassInt i
    parseJSON (Number (D d)) = pure $ PassDouble d
    parseJSON (String t) = case parseJsonDate t of
        Just d  -> pure $ PassDate d
        Nothing -> pure $ PassText t
    parseJSON _ = fail "Could not parse pass field value"

instance FromJSON PassField where
    parseJSON (Object v) =
        PassField             <$>
        v .:? "changeMessage" <*>
        v .: "key"            <*>
        v .:? "label"         <*>
        v .:? "textAlignment" <*>
        v .: "value"          <*>
        v .:? "dateStyle"     <*>
        v .:? "timeStyle"     <*>
        v .:? "isRelative"    <*>
        v .:? "currencyCode"  <*>
        v .:? "numberStyle"
    parseJSON _ = mzero

instance FromJSON RelevantDate where
    parseJSON (String t) = case parseJsonDate t of
        (Just d) -> pure $ RelevantDate d
        Nothing  -> fail "Could not parse relevant date"
    parseJSON _ = mzero

$(deriveFromJSON id ''PassContent)

-- |Tries to parse a web service
parseWebService :: Maybe Text -> Maybe Text -> Maybe WebService
parseWebService Nothing _ = Nothing
parseWebService _ Nothing = Nothing
parseWebService (Just token) (Just url) = Just $ WebService token url

-- |Parses an RGBColor. This is not piped through 'rgb', thus it is not
--  checked whether the specified colour values are in range.
parseRGB :: Parser RGBColor
parseRGB = RGB <$> ("rgb(" .*> decimal)
               <*> ("," .*> decimal)
               <*> ("," .*> decimal)

instance FromJSON RGBColor where
    parseJSON (String t) = case parseOnly parseRGB t of
        Left f -> fail f
        Right r -> pure r
    parseJSON _ = mzero

instance FromJSON PassType where
    parseJSON (Object v)
        | HM.member "boardingPass" v =
            withValue "boardingPass" $ \val -> case val of
              Object o -> BoardingPass <$> o .: "transitType"
                                       <*> parseJSON val
              _ -> fail "Could not parse Boarding Pass"
        | HM.member "coupon" v =
            withValue "coupon" $ \o -> Coupon <$> parseJSON o
        | HM.member "eventTicket" v =
            withValue "eventTicket" $ \o -> Event <$> parseJSON o
        | HM.member "storeCard" v =
            withValue "storeCard" $ \o -> StoreCard <$> parseJSON o
        | HM.member "generic" v =
            withValue "generic" $ \o -> GenericPass <$> parseJSON o
      where
        withValue k f= f $ v HM.! k

instance FromJSON Pass where
    parseJSON o@(Object v) =
        Pass <$>
        v .: "description"                <*>
        v .: "organizationName"           <*>
        v .: "passTypeIdentifier"         <*>
        v .: "serialNumber"               <*>
        v .: "teamIdentifier"             <*>
        v .: "associatedStoreIdentifiers" <*>
        v .: "locations"                  <*>
        v .:? "relevantDate"              <*>
        v .:? "barcode"                   <*>
        v .:? "backgroundColor"           <*>
        v .:? "foregroundColor"           <*>
        v .:? "labelColor"                <*>
        v .:? "logoText"                  <*>
        v .:? "suppressStripShine"        <*>
        wbs                               <*>
        parseJSON o
      where
        wbs = parseWebService <$> v .:? "authenticationToken"
                              <*> v .:? "webServiceURL"


-- * Auxiliary functions

-- |This function takes a 'Text' and a 'BarcodeFormat' and uses the text
--  for both the barcode message and the alternative text.
mkBarcode :: Text -> BarcodeFormat -> Barcode
mkBarcode m f = Barcode (Just m) f m "iso-8859-1"


-- |Creates a @Just RGBColor@ if all supplied numbers are between 0 and 255.
rgb :: (Int, Int, Int) -> Maybe RGBColor
rgb (r, g, b) | isInRange r && isInRange b && isInRange b = Just $ RGB r g b
              | otherwise = Nothing
  where
    isInRange x = 0 <= x && x <= 255

-- |Creates a simple 'PassField' with just a key, a value and an optional label.
--  All the other optional fields are set to 'Nothing'.
mkSimpleField :: Text -- ^ Key
              -> PassValue -- ^ Value
              -> Maybe Text -- ^ Label
              -> PassField
mkSimpleField k v l = PassField Nothing k l Nothing v Nothing Nothing
                                Nothing Nothing Nothing
