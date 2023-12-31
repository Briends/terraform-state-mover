{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( ResourceChanges (..),
    ResourceChange (..),
    Change (..),
    ChangeMapping,
    onlyAddressChanged,
    printResources,
    backupTerraformState,
    generateAndParsePlan,
    moveState,
  )
where

import ConsoleColors (addColor, deleteColor, withColor)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map hiding (map, null)
import System.IO.Temp
import System.Process

type Address = String

newtype ResourceChanges = ResourceChanges {resourceChanges :: [ResourceChange]} deriving (Show)

data ResourceChange = ResourceChange
  { address :: Address,
    change :: Change
  }
  deriving (Show)

data Change = Change
  { actions :: [String],
    before :: Maybe Value,
    after :: Maybe Value
  }
  deriving (Show)

instance FromJSON ResourceChanges where
  parseJSON = withObject "ResourceChanges" $ \v -> ResourceChanges <$> v .: "resource_changes"

instance FromJSON ResourceChange where
  parseJSON = withObject "ResourceChange" $ \v ->
    ResourceChange
      <$> v
      .: "address"
      <*> v
      .: "change"

instance FromJSON Change where
  parseJSON = withObject "Change" $ \v ->
    Change
      <$> v
      .: "actions"
      <*> v
      .:? "before"
      <*> v
      .:? "after"

type ChangeMapping = Map Address Change

onlyAddressChanged :: ChangeMapping -> Address -> Address -> Bool
onlyAddressChanged changeMapping old new =
  let oldChange = changeMapping ! old
      newChange = changeMapping ! new
   in (removeId <$> before oldChange) == (removeId <$> after newChange)

removeId :: Value -> Value
removeId (Object o) = Object $ KeyMap.delete "id" o
removeId _ = error "Expected object"

printResources :: Address -> Address -> IO ()
printResources oldResourceName newResourceName = do
  withColor deleteColor $ putStrLn ("- " <> oldResourceName)
  withColor addColor $ putStrLn ("+ " <> newResourceName)
  putStrLn "-------------"

backupTerraformState :: IO ()
backupTerraformState = do
  putStrLn "Backing up terraform state"
  callCommand "terraform state pull > \"terraform.tfstate.$(date +%Y%m%d%H%M%S)\""

generateAndParsePlan :: IO ResourceChanges
generateAndParsePlan = withSystemTempDirectory "terrform-state-mover" $ \tmpdir -> do
  let tfplanFile = tmpdir <> "/tfplan"
  let tfplanJsonFile = tmpdir <> "/tfplan.json"
  callCommand ("terraform plan -out=" <> tfplanFile <> " --parallelism=100")
  callCommand ("terraform show -json " <> tfplanFile <> (" > " <> tfplanJsonFile))
  throwDecode =<< BL.readFile tfplanJsonFile

moveState :: Address -> Address -> IO ()
moveState oldResourceName newResourceName =
  callCommand ("terraform state mv " <> singlequoted oldResourceName <> (" " <> singlequoted newResourceName))
  where
    singlequoted str = "'" <> str <> "'"
