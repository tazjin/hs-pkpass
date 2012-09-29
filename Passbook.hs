{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


{- |This module provides two functions to sign a 'Pass' using Apple's @signpass@
    command-line tool.

    I intend to move the signing process to a Haskell native OpenSSL binding later
    on, but due to time constraints didn't get around to it yet.

    One function creates a random UUID during the signing process, uses this UUID
    as the passes' serial number and returns it along with the path to the signed pass.
    This function is 'signpass' and is what you would want for dynamically generated,
    new passes.

    The other function, 'signpassWithId', takes an existing ID and signs the pass
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

    This module will most likely only work on OS X machines.
-}
module Passbook (signpass, signpassWithId, genPassId) where

import           Data.Aeson
import           Data.Conduit
import           Data.Conduit.Binary     hiding (sinkFile)
import           Data.Conduit.Filesystem
import qualified Data.Text               as ST
import           Data.Text.Lazy          as LT
import           Data.UUID
import           Passbook.Types
import           Prelude                 hiding (FilePath)
import           Shelly
import           System.Directory        (doesFileExist)
import           System.Random
default (LT.Text)

-- |This function takes a 'Pass' and the path to the asset-folder.
--  The pass is rendered, stored in the asset folder, signed and packed.
--  Afterwards, the 'FilePath' of the signed pkpass file is returned. If
--  an error occured, Nothing is returned.
--  If not icon file is included, Nothing is returned as well.
--  Trailing slashes must be included in the FilePaths.
signpass :: FilePath -- ^ Input file path (asset directory)
         -> FilePath -- ^ Output file path
         -> Pass -- ^ The pass to sign
         -> IO (FilePath, ST.Text) -- ^ The filepath of the signed .pkpass and its UUID
signpass passIn passOut pass = do
    passId <- genPassId
    passPath <- signpassWithId passId passIn passOut pass
    return (passPath, passId)

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
