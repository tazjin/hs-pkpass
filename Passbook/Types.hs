{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE DeriveDataTypeable        #-}

{-# OPTIONS_HADDOCK -ignore-exports #-}


{- |This module provides types and functions for type-safe generation of PassBook's @pass.json@ files.

    This is a complete implementation of the Passbook Package Format Reference, available at
    <https://developer.apple.com/library/ios/#documentation/UserExperience/Reference/PassKit_Bundle/Chapters/Introduction.html>.


    It ensures that passes are created correctly wherever possible. Currently, NSBundle localization is not supported.
-}
module Passbook.Types where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Typeable
import           Data.Text             (Text, pack)
import           Data.Time
import           System.Locale
import           Text.Shakespeare.Text

-- | Auxiliary class to ensure that field values are rendered correctly
class (Typeable a, ToJSON a) => ToPassField a

instance ToPassField Int
instance ToPassField Double
instance ToPassField PassDate
instance ToPassField Text --where

-- * Passbook data types

type Encoding = Text
type Message  = Text

-- |A location field
data Location = Location {
      latitude     :: Double -- ^ Latitude, in degrees, of the location (required)
    , longitude    :: Double -- ^ Longitude, in degrees, of the location (required)
    , altitude     :: Maybe Double -- ^ Altitude, in meters, of the location (optional)
    , relevantText :: Maybe Text -- ^ Text displayed on the lock screen when the pass is relevant (optional)
} deriving (Typeable)

-- |A simple RGB color value. In combination with the 'rgb' function this can be written just like in
--  CSS, e.g. @rgb(43, 53, 65)@. The 'rgb' function also ensures that the provided values are valid.
data RGBColor = RGB Int Int Int
    deriving (Typeable)

-- |Barcode is constructed by a Barcode format, an encoding
--  type and the Barcode message.
data BarcodeFormat = QRCode
                   | PDF417
                   | Aztec
    deriving (Typeable)

-- |A pass barcode. In most cases the helper function 'mkBarcode' should be sufficient.
data Barcode = Barcode {
      altText         :: Maybe Text -- ^ Text displayed near the barcode (optional)
    , format          :: BarcodeFormat -- ^ Barcode format (required)
    , message         :: Text -- ^ Message / payload to be displayed as a barcode (required)
    , messageEncoding :: Text -- ^ Barcode encoding. Default in the mkBarcode functions is iso-8859-1 (required)
} deriving (Typeable)

-- |Pass field alignment
data Alignment = LeftAlign
               | Center
               | RightAlign
               | Natural
    deriving (Typeable)

-- |Pass field date/time display style
data DateTimeStyle = None -- ^ Corresponds to @NSDateFormatterNoStyle@
                   | Short -- ^ Corresponds to @NSDateFormatterShortStyle@
                   | Medium -- ^ Corresponds to @NSDateFormatterMediumStyle@
                   | Long -- ^ Corresponds to @NSDateFormatterLongStyle@
                   | Full -- ^ Corresponds to @NSDateFormatterFullStyle@
    deriving (Typeable)

-- |Pass field number display style
data NumberStyle = Decimal
                 | Percent
                 | Scientific
                 | SpellOut
    deriving (Typeable)

-- |A single pass field. The 'value' of a 'PassField' can be anything that is an instance of 'ToPassField'.
--  Dates and numbers are always correctly formatted. If you add another type to 'ToPassField' please make sure
--  that it's corresponding 'ToJSON' instance generates Passbook-compatible JSON.
--  To create a very simple key/value field containing text you can use the 'mkSimpleField' function.
data PassField = forall a . ToPassField a => PassField {
    -- standard field keys
      changeMessage :: Maybe Text -- ^ Message displayed when the pass is updated. May contain the @%\@@ placeholder for the value. (optional)
    , key           :: Text -- ^ Must be a unique key within the scope of the pass (e.g. \"departure-gate\") (required)
    , label         :: Maybe Text -- ^ Label text for the field. (optional)
    , textAlignment :: Maybe Alignment -- ^ Alignment for the field's contents. Not allowed for primary fields. (optional)
    , value         :: a -- ^ Value of the field. Must be a string, ISO 8601 date or a number. (required)

    -- Date style keys (all optional). If any key is present, the field will be treated as a date.
    , dateStyle     :: Maybe DateTimeStyle -- ^ Style of date to display (optional)
    , timeStyle     :: Maybe DateTimeStyle -- ^ Style of time to display (optional)
    , isRelative    :: Maybe Bool -- ^ Is the date/time displayed relative to the current time or absolute? Default: @False@ (optional)

    -- Number style keys (all optional). Not allowed if the field is not a number.
    , currencyCode  :: Maybe Text -- ^ ISO 4217 currency code for the field's value (optional)
    , numberStyle   :: Maybe NumberStyle -- ^ Style of number to display. See @NSNumberFormatterStyle@ docs for more information. (optional)
} deriving (Typeable)

-- |BoardingPass transit type. Only necessary for Boarding Passes.
data TransitType = Air
                 | Boat
                 | Bus
                 | Train
                 | GenericTransit
    deriving (Typeable)

-- |Newtype wrapper around 'UTCTime' with a 'ToJSON' instance that ensures Passbook-compatible
--  time rendering. (ISO 8601)
newtype PassDate = PassDate UTCTime deriving (Typeable)

-- |The type of a pass including the specific auxiliary, main, etc. fields
data PassType = BoardingPass TransitType PassContent
              | Coupon PassContent
              | Event PassContent
              | GenericPass PassContent
              | StoreCard PassContent
    deriving (Typeable)

data WebService = WebService {
      authenticationToken        :: Text -- ^ Authentication token for use with the web service. Must be 16 characters or longer (optional)
    , webServiceURL              :: Text -- ^ The URL of a web service that conforms to the API described in the Passbook Web Service Reference (optional)
} deriving (Typeable)

-- |The fields within a pass
data PassContent = PassContent {
      headerFields    :: [PassField] -- ^ Fields to be displayed on the front of the pass. Always shown in the stack.
    , primaryFields   :: [PassField] -- ^ Fields to be displayed prominently on the front of the pass.
    , secondaryFields :: [PassField] -- ^ Fields to be displayed on the front of the pass.
    , auxiliaryFields :: [PassField] -- ^ Additional fields to be displayed on the front of the pass.
    , backFields      :: [PassField] -- ^ Fields to be on the back of the pass.
} deriving (Typeable)

-- |A complete pass
data Pass = Pass {
    -- Required keys
      description                :: Text -- ^ Brief description of the pass (required)
    , formatVersion              :: Int  -- ^ Version of the file format. The value must be 1. (required)
    , organizationName           :: Text -- ^ Display name of the organization that signed the pass (required)
    , passTypeIdentifier         :: Text -- ^ Pass type identifier, as issued by Apple (required)
    , serialNumber               :: Text -- ^ Unique serial number for the pass (required)
    , teamIdentifier             :: Text -- ^ Team identifier for the organization (required)

    -- associated app keys
    , associatedStoreIdentifiers :: [Text] -- ^ A list of iTunes Store item identifiers for associated apps (optional)

    -- relevance keys
    , locations                  :: [Location]  -- ^ Locations where the pass is relevant (e.g. that of a store) (optional)
    , relevantDate               :: Maybe PassDate -- ^ ISO 8601 formatted date for when the pass becomes relevant (optional)

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
} deriving (Typeable)

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
                    , "formatVersion" .= (1 :: Int) -- Harcoding this because it should not be changed
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

instance Show PassDate where
    show (PassDate d) =
        let timeFormat = iso8601DateFormat $ Just $ timeFmt defaultTimeLocale
        in  formatTime defaultTimeLocale timeFormat d

instance ToJSON PassDate where
    toJSON = toJSON . pack . show

instance Show PassType where
    show (BoardingPass _ _) = "boardingPass"
    show (Coupon _) = "coupon"
    show (Event _) = "eventTicket"
    show (GenericPass _) = "generic"
    show (StoreCard _) = "storeCard"

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
mkSimpleField :: ToPassField a
              => Text -- ^ Key
              -> a -- ^ Value
              -> Maybe Text -- ^ Label
              -> PassField
mkSimpleField k v l = PassField Nothing k l Nothing v Nothing Nothing
                                Nothing Nothing Nothing

