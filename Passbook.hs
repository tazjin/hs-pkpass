{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


{- |This module provides different functions to sign a 'Pass' using Apple's @signpass@
    command-line tool.

    I intend to move the signing process to a Haskell native OpenSSL binding later
    on, but due to time constraints didn't get around to it yet.

    If you want to use this module with an existing .pkpass file, you can import its
    @pass.json@ file using the function 'loadPass'.

    The function 'signpass' creates a random UUID during the signing process, uses this UUID
    as the passes' serial number and returns it along with the path to the signed pass.

    The funciton 'signpassWithModifier' creates a random UUID during the signing process,
    passes this UUID on to a function that modifies the pass accordingly (e.g. sets the serial
    number or the barcode payload to the UUID) and otherwise works like 'signpass'.
    The function 'updateBarcode' is provided as well.

    The function, 'signpassWithId', takes an existing ID and signs the pass
    using that for the serial number and file name. This will most likely be used
    for updating existing passes.

    Using the function is very simple, assuming you have created a 'Pass' called
    @myPass@ and you have the related assets (e.g. the logo.png and icon.png files)
    stored in a folder named @myPass/@.

    You want the signed pass to be stored in a folder called @passes/@. You call
    'signpass' like this:

   > (path, passId) <- signpass "myPass" "passes" myPass

    You will find the pass at @path@ with the filename @passId.pkpass@. Using the
    types from "Passbook.Types" ensures that passes are generated correctly.

    The @signpass@ utility must have access to your Keychain to be able to retrieve
    your Pass Type Certificate for the pass to sign. You should run it once outside
    of any program to grant it full access to your Keychain.

    Please note that an @icon.png@ file /must be/ present in your asset folder,
    otherwise the generated pass will not work.

    Refer to Apple's Passbook documentation at <https://developer.apple.com/passbook/>
    for more information or to retrieve the @signpass@ tool which is included in the
    Passbook Support Materials. (iOS Developer Membership necessary)

    This module will most likely only work on OS X machines.
-}
module Passbook ( signpass
                , signpassWithId
                , signpassWithModifier
                , genPassId
                , updateBarcode
                , loadPass ) where

import           Codec.Archive.Zip
import           Control.Monad             (liftM)
import           Data.Aeson
import           Data.ByteString.Lazy      as LB
import           Data.Conduit
import           Data.Conduit.Binary       hiding (sinkFile)
import           Data.Conduit.Filesystem
import qualified Data.Text                 as ST
import           Data.Text.Lazy            as LT
import           Data.UUID
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
signpassWithId :: ST.Text -- ^ The pass ID
               -> FilePath -- ^ Input file path (asset directory)
               -> FilePath -- ^ Output file path
               -> Pass -- ^ The pass to sign
               -> IO FilePath
signpassWithId passId passIn passOut pass = shelly $ do
    let tmp = passOut </> passId
        lazyId = fromStrict passId
    cp_r passIn tmp
    liftIO $ renderPass (tmp </> "pass.json") pass { serialNumber = passId }
    signcmd lazyId tmp passOut
    rm_rf tmp
    return (passOut </> (LT.append lazyId ".pkpass"))

-- |Generates a random UUID for a Pass using "Data.UUID" and "System.Random"
genPassId :: IO ST.Text
genPassId = randomIO >>= return . ST.pack . toString

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
                    , "-o", toTextIgnore $passOut </> (LT.append uuid ".pkpass") ] -- Name of the output file

-- |Tries to parse the pass.json file contained in a .pkpass into a valid
--  'Pass'. If Passbook accepts the .pkpass file, this function should never
--  return @Nothing@.
loadPass :: FilePath -- ^ Location of the .pkpass file
         -> IO (Maybe Pass)
loadPass path = do
    archive <- (liftM toArchive) $ LB.readFile $ encodeString path
    case findEntryByPath "pass.json" archive of
        Nothing   -> return Nothing
        Just pass -> return $ decode $ fromEntry pass


