{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


{- |This module provides different functions to sign a Passbook 'Pass'. Some of these functions
    only work on OS X and only if Apple's @signpass@ utility is installed. Please refer to the
    documentation regarding the specific function.

    If you want to use this module with an existing .pkpass file, you can import its
    @pass.json@ file using the function 'loadPass'.

    The function 'signOpen' signs a 'Pass' using OpenSSL on any Unix-based OS.

    The function 'signpass' creates a random UUID during the signing process, uses this UUID
    as the passes' serial number and returns it along with the path to the signed pass.

    The funciton 'signpassWithModifier' creates a random UUID during the signing process,
    passes this UUID on to a function that modifies the pass accordingly (e.g. sets the serial
    number or the barcode payload to the UUID) and otherwise works like 'signpass'.
    The function 'updateBarcode' is provided as well.

    The function, 'signpassWithId', takes an existing ID and signs the pass
    using that for the serial number and file name. This will most likely be used
    for updating existing passes.

    Using these function is very simple, assuming you have created a 'Pass' called
    @myPass@ and you have the related assets (e.g. the logo.png and icon.png files)
    stored in a folder named @myPass/@.

    You want the signed pass to be stored in a folder called @passes/@. You call
    'signpass' like this:

   > (path, passId) <- signpass "myPass" "passes" myPass

    You will find the pass at @path@ with the filename @passId.pkpass@. Using the
    types from "Passbook.Types" ensures that passes are generated correctly.

    If you are using the functions that depend on Apple's @signpass@ utility,
    it must have access to your Keychain to be able to retrieve your Pass Type
    Certificate for the pass to sign. You should run it once on the Terminal to
    grant it the required access rights (OS X prompts you automatically).

    Please note that an @icon.png@ file /must be/ present in your asset folder,
    otherwise the generated pass will not work. This is /not/ checked by this module.

    Refer to Apple's Passbook documentation at <https://developer.apple.com/passbook/>
    for more information or to retrieve the @signpass@ tool which is included in the
    Passbook Support Materials. (iOS Developer Membership necessary)

-}
module Passbook ( -- * Sign using signpass
                  signpass
                , signpassWithId
                , signpassWithModifier
                  -- * Sign using OpenSSL
                , signOpen
                  -- * Helper functions
                , genPassId
                , updateBarcode
                , loadPass
                , module Passbook.Types ) where

import           Codec.Archive.Zip
import           Control.Monad             (liftM)
import           Control.Monad.IO.Class    (liftIO)
import           Data.Aeson
import qualified Data.ByteString.Lazy      as LB
import           Data.Conduit
import           Data.Conduit.Binary       hiding (sinkFile)
import           Data.Conduit.Filesystem
import           Data.Text.Lazy                 (Text)
import qualified Data.Text                 as ST
import qualified Data.Text.Lazy            as LT
import           Data.UUID
import Filesystem.Path (filename)
import           Filesystem.Path.CurrentOS (encodeString)
import           Passbook.Types
import           Prelude                   hiding (FilePath)
import           Shelly
import           System.Directory          (doesFileExist)
import           System.Random

default (LT.Text)

-- |Takes the filepaths to the folder containing the path assets
--  and the output folder, a 'Pass' and uses a random UUID to
--  create and sign the pass.
--
--  /Important:/ OS X only!
signpass :: FilePath -- ^ Input file path (asset directory)
         -> FilePath -- ^ Output file path
         -> Pass -- ^ The pass to sign
         -> IO (FilePath, ST.Text) -- ^ The filepath of the signed .pkpass and its UUID
signpass passIn passOut pass = do
    passId <- genPassId
    passPath <- signpassWithId passId passIn passOut pass
    return (passPath, passId)

-- |Works like 'signpass', except for the fourth argument which is a
--  modifier function that updates the pass with the generated UUID.
--  This is useful for cases where you want to store the UUID in the barcode
--  or some other field on the pass as well.
--
--  /Important:/ OS X only!
signpassWithModifier :: FilePath -- ^ Input file path (asset directory)
                     -> FilePath -- ^ Output file path
                     -> Pass -- ^ The pass to sign
                     -> (ST.Text -> Pass -> Pass) -- ^ Modifier function
                     -> IO (FilePath, ST.Text) -- ^ The filepath of the signed .pkpass and its UUID
signpassWithModifier passIn passOut pass modifier = do
    passId <- genPassId
    passPath <- signpassWithId passId passIn passOut $ modifier passId pass
    return (passPath, passId)

-- |Updates the barcode in a pass with the UUID. This can be passed to 'signpassWithModifier'
updateBarcode :: ST.Text -> Pass -> Pass
updateBarcode n p = case barcode p of
    Nothing -> p -- This pass has no barcode.
    Just ob -> p { barcode = Just ob { altText = Just n
                                     , message = n } }

-- |Creates and signs a 'Pass' with an existing ID.
--
--  /Important:/ OS X only!
signpassWithId :: ST.Text -- ^ The pass ID
               -> FilePath -- ^ Input file path (asset directory)
               -> FilePath -- ^ Output file path
               -> Pass -- ^ The pass to sign
               -> IO FilePath
signpassWithId passId passIn passOut pass = shelly $ do
    let tmp = passOut </> passId
        lazyId = LT.fromStrict passId
    cp_r passIn tmp
    liftIO $ renderPass (tmp </> "pass.json") pass { serialNumber = passId }
    signcmd lazyId tmp passOut
    rm_rf tmp
    return (passOut </> LT.append lazyId ".pkpass")

-- |Helper function to generate a hash
genHash :: FilePath -> Sh (Text, Text)
genHash file = do
    rawhash <- run "openssl" ["sha1", toTextIgnore file]
    let hash = LT.drop 1 $ LT.dropWhile (/= ' ') rawhash
    return (toTextIgnore $ filename file, LT.filter (/= '\n') hash)

-- |Render JSON and put it in a file
saveJSON :: ToJSON a => a -> FilePath -> IO ()
saveJSON json path = LB.writeFile (LT.unpack $ toTextIgnore path) $ encode json

-- |Helper function to sign the manifest
sslSign :: FilePath -- ^ Certificate
        -> FilePath -- ^ Key
        -> FilePath -- ^ Temporary directory containing manifest.json
        -> Sh Text
sslSign cert key  tmp =
    run "openssl" [ "smime", "-binary"
                  , "-sign"
                  , "-signer", toTextIgnore cert
                  , "-inkey" , toTextIgnore key
                  , "-in", "manifest.json"
                  , "-out", "signature"
                  , "-outform", "DER" ]

-- | Signs a pass using openssl. You need to export your certificate from the keychain.
--   Assuming you have saved the certificate as @cert.p12@, the conversion works like this:
-- 
-- > $ openssl pkcs12 -in cert.p12 -clcerts -nokeys -out certificate.pem 
-- > $ openssl pkcs12 -in cert.p12 -nocerts -out keypw.pem
--
--   Enter a password for your key file, you will only need this once in the next step.
--   Then strip the password from your key file using:
--
-- > $ openssl rsa -in keypw.pem -out key.pem
--
--   /Important:/ All paths passed to this function /must/ be absolute! This function works
--   on all Unix operating systems.
signOpen :: FilePath -- ^ Input file path (asset directory)
         -> FilePath -- ^ Output folder
         -> FilePath -- ^ Certificate
         -> FilePath -- ^ Certificate key
         -> Pass     -- ^ The pass to sign
         -> IO FilePath -- ^ The signed .pkpass file
signOpen passIn passOut cert key pass = shelly $ silently $ do
    let tmp = passOut </> (serialNumber pass)
        passFile = LT.append (LT.fromStrict $ serialNumber pass) ".pkpass"
    cp_r passIn tmp
    liftIO $ renderPass (tmp </> "pass.json") pass
    cd tmp
    manifest <- liftM Manifest $ pwd >>= ls >>= mapM genHash
    liftIO $ saveJSON manifest (tmp </> "manifest.json")
    sslSign cert key tmp
    files <- liftM (map (toTextIgnore . filename)) $ ls =<< pwd
    run "zip" ((toTextIgnore $ passOut </> passFile) : files)
    rm_rf tmp
    return (passOut </> passFile)

-- |Generates a random UUID for a Pass using "Data.UUID" and "System.Random"
genPassId :: IO ST.Text
genPassId = liftM (ST.pack . toString) randomIO

-- |Render and store a pass.json at the desired location.
renderPass :: FilePath -> Pass -> IO ()
renderPass path pass =
    let rendered = sourceLbs $ encode pass
    in runResourceT $ rendered $$ sinkFile path

-- |Call the signpass tool.
signcmd :: Text -- ^ The pass identifier / serial number to uniquely identify the pass
        -> FilePath -- ^ The temporary asset folder.
        -> FilePath -- ^ The output folder for all .pkpass files
        -> Sh ()
signcmd uuid assetFolder passOut =
    run_ "signpass" [ "-p", toTextIgnore assetFolder -- The input folder
                    , "-o", toTextIgnore $ passOut </> LT.append uuid ".pkpass" ] -- Name of the output file

-- |Tries to parse the pass.json file contained in a .pkpass into a valid
--  'Pass'. If Passbook accepts the .pkpass file, this function should never
--  return @Nothing@.
loadPass :: FilePath -- ^ Location of the .pkpass file
         -> IO (Maybe Pass)
loadPass path = do
    archive <- liftM toArchive $ LB.readFile $ encodeString path
    case findEntryByPath "pass.json" archive of
        Nothing   -> return Nothing
        Just pass -> return $ decode $ fromEntry pass
