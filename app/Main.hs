{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}

import Control.Monad.IO.Class (liftIO)
import Data.Text (pack)
import Database.Selda
import Database.Selda.SQLite
import System.Directory (doesFileExist)
import Text.Read (readMaybe)

data Card = Card 
  { group :: Text
  , name  :: Text
  , hp    :: Maybe Int }
  deriving Generic
instance SqlRow Card

file = "cards.sqlite"

main :: IO ()
main = do
  putStrLn "(1) Add hero | (2) Add villain | (3) Add environment | (4) List cards"
  option <- getLine
  case readMaybe option :: Maybe Int of
    Just 1 -> do 
      putStrLn "Enter hero name: "
      name <- getLine
      putStrLn "Enter hero HP: "
      hp <- getLine
      heroInsert (pack name) (Just (read hp :: Int)) 
      main
    Just 2 -> do 
      putStrLn "Enter villain name: "
      name <- getLine
      putStrLn "Enter villain HP: "
      hp <- getLine
      villainInsert (pack name) (Just (read hp :: Int)) 
      main
    Just 3 -> do 
      putStrLn "Enter environment name: "
      name <- getLine
      envInsert (pack name) Nothing
      main
    Just 4 -> printCards >> main
    _ -> putStrLn "Invalid option." >> main

-- General card insertion
cards :: Table Card
cards = table "cards" [#name :- primary]

cardInsert :: Text -> Text -> Maybe Int -> IO ()
cardInsert group name hp = withSQLite file $ do
  tryCreateTable cards
  result <- tryInsert cards
      [ Card group name hp ]
  liftIO $ putStrLn $ 
    if result
    then "Successfully created " <> show name <> " with " <> show hp <> " HP." 
    else "Error creating entry."

-- Insert cards by group
hero    = "hero"
villain = "villain"
env     = "env"

heroInsert    = cardInsert hero
villainInsert = cardInsert villain 
envInsert     = cardInsert env 

printCards :: IO ()
printCards = withSQLite file $ do
  list <- query $ do
    card <- select cards
    return (card ! #group :*: card ! #name :*: card ! #hp)
  liftIO $ print list 

