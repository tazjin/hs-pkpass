{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module PKPass where

import PKPass.Types
import Shelly
import Data.Text.Lazy as LT
import System.Directory (doesFileExist)
import Data.Aeson
import Data.Conduit
import Data.Conduit.Binary hiding (sinkFile)
import Data.Conduit.Filesystem
import Data.UUID
import System.Random
import Prelude hiding (FilePath)
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
         -> IO (FilePath, Text) -- ^ The filepath of the signed .pkpass and its UUID
signpass passIn passOut pass = shelly $ do
    uuid <- liftIO genPassID
    let tmp = passOut </> uuid
    cp_r passIn tmp
    liftIO $ renderPass (tmp </> "pass.json") pass
    signcmd uuid tmp passOut
    rm_rf tmp
    return (passOut </> (LT.append uuid ".pkpass"), uuid)

-- |Generates a random UUID for a Pass using "Data.UUID" and "System.Random"
genPassID :: IO Text
genPassID = randomIO >>= return . pack . toString

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