module Main (main) where

import Control.Monad
import Data.List ((\\))
import Data.Map hiding (map, null, (\\))
import Lib
import System.Process

main :: IO ()
main = do
  putStrLn "Running terraform init"
  callCommand "terraform init"

  -- Extract old and new resource names from the JSON plan
  ResourceChanges changes <- generateAndParsePlan

  let changeMapping = fromList [(address r, change r) | r <- changes] :: ChangeMapping
  let oldResourceNames = [address r | r <- changes, "delete" `elem` actions (change r)]
  let newResourceNames = [address r | r <- changes, "create" `elem` actions (change r)]
  let movedResources = [(old, new) | old <- oldResourceNames, new <- newResourceNames, onlyAddressChanged changeMapping old new]
  let deletedNotMovedResources = oldResourceNames \\ fmap fst movedResources
  let createdNotMovedResourcs = newResourceNames \\ fmap snd movedResources

  forM_ movedResources $ uncurry printResources

  unless (null deletedNotMovedResources || null createdNotMovedResourcs) $ do
    withColor warnColor $ do
      forM_ deletedNotMovedResources $ \r -> putStrLn ("- " <> r)
      forM_ createdNotMovedResourcs $ \r -> putStrLn ("+ " <> r)
    putStrLn "-------------"

  putStrLn ("Old resources: " <> show (length oldResourceNames))
  putStrLn ("New resources: " <> show (length newResourceNames))
  putStrLn ("Moved resources: " <> show (length movedResources))
  putStrLn ("Not moved resources: " <> show (length deletedNotMovedResources + length createdNotMovedResourcs))

  -- Move state for each old resource to its corresponding new resource
  unless (null movedResources) $ do
    putStrLn "Do you want to move state for the above resources? (y/n)"
    answer <- getLine
    when (answer == "y" || answer == "yes") $ do
      backupTerraformState
      forM_ movedResources $ uncurry moveState
      putStrLn "State moved successfully"
