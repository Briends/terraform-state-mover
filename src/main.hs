{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.List (length, union, (\\))
import Data.Map hiding (map, null)
import Data.Maybe
import Data.Text (pack, replace, unpack)
import System.Console.ANSI
import System.Directory (doesFileExist)
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

main :: IO ()
main = do
  putStrLn "Running terraform init"
  callCommand "terraform init"

  -- Extract old and new resource names from the JSON plan
  ResourceChanges resourceChanges <- generateAndParsePlan

  let changeMapping = fromList [(address r, change r) | r <- resourceChanges] :: ChangeMapping
  let oldResourceNames = [address r | r <- resourceChanges, "delete" `elem` actions (change r)]
  let newResourceNames = [address r | r <- resourceChanges, "create" `elem` actions (change r)]
  let movedResources = [(old, new) | old <- oldResourceNames, new <- newResourceNames, onlyAddressChanged changeMapping old new]
  let deletedNotMovedResources = oldResourceNames Data.List.\\ map fst movedResources
  let createdNotMovedResourcs = newResourceNames Data.List.\\ map snd movedResources

  forM_ movedResources $ uncurry printResources

  unless (null deletedNotMovedResources || null createdNotMovedResourcs) $ do
    withColor warnColor $ do
      forM_ deletedNotMovedResources $ \r -> putStrLn $ "- " ++ r
      forM_ createdNotMovedResourcs $ \r -> putStrLn $ "+ " ++ r
    putStrLn "-------------"

  putStrLn $ "Old resources: " ++ show (length oldResourceNames)
  putStrLn $ "New resources: " ++ show (length newResourceNames)
  putStrLn $ "Moved resources: " ++ show (length movedResources)
  putStrLn $ "Not moved resources: " ++ show (length deletedNotMovedResources + length createdNotMovedResourcs)

  -- Move state for each old resource to its corresponding new resource
  unless (null movedResources) $ do
    putStrLn "Do you want to move state for the above resources? (y/n)"
    answer <- getLine
    when (answer == "y" || answer == "yes") $ do
      backupTerraformState
      forM_ movedResources $ uncurry moveState
      putStrLn "State moved successfully"

onlyAddressChanged :: ChangeMapping -> Address -> Address -> Bool
onlyAddressChanged changeMapping old new =
  let oldChange = changeMapping ! old
      newChange = changeMapping ! new
   in (removeId <$> before oldChange) == (removeId <$> after newChange)

removeId :: Value -> Value
removeId (Object o) = Object $ KeyMap.delete "id" o

printResources :: Address -> Address -> IO ()
printResources oldResourceName newResourceName = do
  withColor deleteColor $ putStrLn $ "- " ++ oldResourceName
  withColor addColor $ putStrLn $ "+ " ++ newResourceName
  putStrLn "-------------"

backupTerraformState :: IO ()
backupTerraformState = do
  putStrLn "Backing up terraform state"
  callCommand "terraform state pull > \"terraform.tfstate.$(date +%Y%m%d%H%M%S)\""

generateAndParsePlan :: IO ResourceChanges
generateAndParsePlan = withSystemTempDirectory "terrform-state-mover" $ \tmpdir -> do
  let tfplanFile = tmpdir ++ "/tfplan"
  let tfplanJsonFile = tmpdir ++ "/tfplan.json"
  callCommand $ "terraform plan -out=" ++ tfplanFile ++ " --parallelism=100"
  callCommand $ "terraform show -json " ++ tfplanFile ++ " > " ++ tfplanJsonFile
  throwDecode =<< BL.readFile tfplanJsonFile

moveState :: Address -> Address -> IO ()
moveState oldResourceName newResourceName =
  callCommand $ "terraform state mv " ++ singlequoted oldResourceName ++ " " ++ singlequoted newResourceName
  where
    singlequoted str = "'" ++ str ++ "'"

-- Utility for printing colors

deleteColor :: [SGR]
deleteColor = [SetColor Foreground Vivid Red]

addColor :: [SGR]
addColor = [SetColor Foreground Vivid Green]

warnColor :: [SGR]
warnColor = [SetColor Foreground Vivid Yellow]

withColor :: [SGR] -> IO () -> IO ()
withColor color action = do
  setSGR color
  action
  setSGR [Reset]
