{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Humac.PKPass.Types where

import           Data.Aeson
import           Data.Text (Text, pack)
import           Text.Shakespeare.Text

type Encoding = Text
type Message  = Text

-- |A complete pass
data Pass = Pass PassHeader PassType

-- |The type of a pass including the specific auxiliary, main, etc. fields
data PassType = BoardingPass -- ^NYI
              | Coupon
              | Event
              | Generic
              | StoreCard

-- |A location field (incomplete according to spec)
data Location = Location {
      longitude    :: Double
    , latitude     :: Double
    , relevantText :: Maybe Text
}

data RGBColor = RGB Int Int Int

-- |Barcode is constructed by a Barcode format, an encoding
--  type and the Barcode message.
data BarcodeFormat = QRCode
                   | PDF417

data Barcode = Barcode BarcodeFormat Encoding Message

data PassHeader = PassHeader {
      formatVersion       :: Int
    , passTypeIdentifier  :: Text
    , serialNumber        :: Text
    , webServiceURL       :: Text
    , authenticationToken :: Text
    , teamIdentifier      :: Text
    , locations           :: [Location]
    , barcode             :: Barcode
    , organizationName    :: Text
    , description         :: Text
    , logoText            :: Text
    , foregroundColor     :: RGBColor
    , backgroundColor     :: RGBColor
}


-- * JSON instances
renderRGB :: RGBColor -> Text
renderRGB (RGB r g b) = [st|rgb(#{show r},#{show g},#{show b})|]

instance ToJSON RGBColor where
    toJSON = toJSON . renderRGB

instance ToJSON BarcodeFormat where
    toJSON QRCode = toJSON ("PKBarcodeFormatQR" :: Text)
    toJSON PDF417 = toJSON ("PKBarcodeFormatPDF417" :: Text)

{-
PKPass example headers:

  "formatVersion" : 1,
  "passTypeIdentifier" : "pass.com.toytown.membership",
  "serialNumber" : "8j23fm3",
  "webServiceURL" : "https://example.com/passes/",
  "authenticationToken" : "vxwxd7J8AlNNFPS8k0a0FfUFtq0ewzFdc",
  "teamIdentifier" : "A1B2C3D4E5",
  "locations" : [
    {
      "longitude" : -122.3748889,
      "latitude" : 37.6189722
    },
    {
      "longitude" : -122.03118,
      "latitude" : 37.33182
    }
  ],
  "barcode" : {
    "message" : "123456789",
    "format" : "PKBarcodeFormatPDF417",
    "messageEncoding" : "iso-8859-1"
  },
  "organizationName" : "Toy Town",
  "description" : "Membership card",
  "logoText" : "Toy Town",
  "foregroundColor" : "rgb(255, 255, 255)",
  "backgroundColor" : "rgb(197, 31, 31)",

-}
