{-# LANGUAGE OverloadedStrings, QuasiQuotes, ExistentialQuantification, FlexibleInstances, UndecidableInstances, RecordWildCards, TemplateHaskell #-}

-- |This module provides types and functions for type-safe generation of PassBook's @pass.json@ files.
--  It ensures that passes are created correctly wherever possible. Currently, NSBundle localization is not supported.
module PKPass.Types where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text (Text, pack)
import           Data.Time
import           Text.Shakespeare.Text
import System.Locale

type Encoding = Text
type Message  = Text

-- * Auxiliary class to ensure that field values are rendered correctly
class ToJSON a => ToPassField a where
    toPassField :: a -> Value

instance (Num a, ToJSON a) => ToPassField a where
    toPassField = toJSON

instance ToPassField PassDate where
    toPassField = toJSON

instance ToPassField Text where
    toPassField = toJSON

-- * Passbook data types

-- |A location field (incomplete according to spec)
data Location = Location {
      latitude     :: Double -- ^ Latitude, in degrees, of the location (required)
    , longitude    :: Double -- ^ Longitude, in degrees, of the location (required)
    , altitude     :: Maybe Double -- ^ Altitude, in meters, of the location (optional)
    , relevantText :: Maybe Text -- ^ Text displayed on the lock screen when the pass is relevant (optional)
}

data RGBColor = RGB Int Int Int

-- |Barcode is constructed by a Barcode format, an encoding
--  type and the Barcode message.
data BarcodeFormat = QRCode
                   | PDF417
                   | Aztec

data Barcode = Barcode {
      altText :: Maybe Text -- ^ Text displayed near the barcode (optional)
    , format :: BarcodeFormat -- ^ Barcode format (required)
    , message :: Text -- ^ Message / payload to be displayed as a barcode (required)
    , messageEncoding :: Text -- ^ Barcode encoding. Default in the mkBarcode functions is iso-8859-1 (required)
}

-- |Pass field alignment
data Alignment = LeftAlign
               | Center
               | RightAlign
               | Natural

-- |Pass field date/time display style
data DateTimeStyle = None -- ^ Corresponds to @NSDateFormatterNoStyle@
                   | Short -- ^ Corresponds to @NSDateFormatterShortStyle@
                   | Medium -- ^ Corresponds to @NSDateFormatterMediumStyle@
                   | Long -- ^ Corresponds to @NSDateFormatterLongStyle@
                   | Full -- ^ Corresponds to @NSDateFormatterFullStyle@

-- |Pass field number display style
data NumberStyle = Decimal
                 | Percent
                 | Scientific
                 | SpellOut

-- |A single pass field.
data PassField = forall a . ToPassField a => PassField {
    -- * standard field keys
      changeMessage :: Maybe Text -- ^ Message displayed when the pass is updated. May contain the @%\@@ placeholder for the value. (optional)
    , key :: Text -- ^ Must be a unique key within the scope of the pass (e.g. \"departure-gate\") (required)
    , label :: Maybe Text -- ^ Label text for the field. (optional)
    , textAlignment :: Maybe Alignment -- ^ Alignment for the field's contents. Not allowed for primary fields. (optional)
    , value :: a -- ^ Value of the field. Must be a string, ISO 8601 date or a number. (required)

    -- * Date style keys (all optional). If any key is present, the field will be treated as a date.
    , dateStyle :: Maybe DateTimeStyle -- ^ Style of date to display
    , timeStyle :: Maybe DateTimeStyle -- ^ Style of time to display
    , isRelative :: Maybe Bool -- ^ Is the date/time displayed relative to the current time or absolute? Default: @False@

    -- * Number style keys (all optional). Not allowed if the field is not a number.
    , currencyCode :: Maybe Text -- ^ ISO 4217 currency code for the field's value
    , numberStyle  :: Maybe NumberStyle -- ^ Style of number to display. See @NSNumberFormatterStyle@ docs for more information.
}

data TransitType = Air
                 | Boat
                 | Bus
                 | Train
                 | GenericTransit

-- |Newtype wrapper around 'UTCTime' with a 'ToJSON' instance that ensures Passbook-compatible
--  time rendering. (ISO 8601)
newtype PassDate = PassDate UTCTime

-- |The type of a pass including the specific auxiliary, main, etc. fields
data PassType = BoardingPass TransitType PassContent
              | Coupon PassContent
              | Event PassContent
              | GenericPass PassContent
              | StoreCard PassContent

getPassContent :: PassType -> PassContent
getPassContent pc = case pc of
    BoardingPass _ pc -> pc
    Coupon pc         -> pc
    Event pc          -> pc
    GenericPass pc    -> pc
    StoreCard pc      -> pc

instance ToJSON PassType where
    toJSON (BoardingPass tt PassContent{..}) = object ["boardingPass" .= object [
        "transitType" .= tt
      , "headerFields" .= headerFields
      , "primaryFields" .= primaryFields
      , "secondaryFields" .= secondaryFields
      , "auxiliaryFields" .= auxiliaryFields
      , "backFields" .= backFields ]]
    toJSON pt = object [ (pack $ show pt) .= (getPassContent pt) ]

-- |The fields within a pass
data PassContent = PassContent {
      headerFields :: [PassField] -- ^ Fields to be displayed on the front of the pass. Always shown in the stack.
    , primaryFields :: [PassField] -- ^ Fields to be displayed prominently on the front of the pass.
    , secondaryFields :: [PassField] -- ^ Fields to be displayed on the front of the pass.
    , auxiliaryFields :: [PassField] -- ^ Additional fields to be displayed on the front of the pass.
    , backFields :: [PassField] -- ^ Fields to be on the back of the pass.
}

-- |A complete pass
data Pass = Pass {
    -- * Required keys
      description         :: Text -- ^ Brief description of the pass (required)
    , formatVersion       :: Int  -- ^ Version of the file format. The value must be 1. (required)
    , organizationName    :: Text -- ^ Display name of the organization that signed the pass (required)
    , passTypeIdentifier  :: Text -- ^ Pass type identifier, as issued by Apple (required)
    , serialNumber        :: Text -- ^ Unique serial number for the pass (required)
    , teamIdentifier      :: Text -- ^ Team identifier for the organization (required)

    -- * associated app keys
    , associatedStoreIdentifiers :: [Text] -- ^ A list of iTunes Store item identifiers for associated apps (optional)
    
    -- * relevance keys
    , locations :: [Location]  -- ^ Locations where the pass is relevant (e.g. that of a store) (optional)
    , relevantDate :: Maybe PassDate -- ^ ISO 8601 formatted date for when the pass becomes relevant (optional)

    -- * visual appearance key
    , barcode :: Maybe Barcode -- ^ Barcode information (optional)
    , backgroundColor :: Maybe RGBColor -- ^ Background color of the pass (optional)
    , foregroundColor :: Maybe RGBColor -- ^ Foreground color of the pass (optional)
    , labelColor :: Maybe Text -- ^ Color of the label text. If omitted, the color is determined automatically. (optional)
    , logoText :: Maybe Text -- ^ Text displayed next to the logo on the pass (optional)
    , suppressStripShine :: Maybe Bool -- ^ If @True@, the strip image is displayed without a shine effect. (optional)

    -- * web service keys
    , authenticationToken :: Maybe Text -- ^ Authentication token for use with the web service. Must be 16 characters or longer (optional)
    , webServiceURL :: Maybe Text -- ^ The URL of a web service that conforms to the API described in the Passbook Web Service Reference

    , passContent :: PassType -- ^ The kind of pass and the passes' fields
}

-- * JSON instances

$(deriveToJSON id ''Location)
$(deriveToJSON id ''Barcode)
$(deriveToJSON id ''PassField)
$(deriveToJSON id ''PassContent)
$(deriveToJSON id ''Pass)

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