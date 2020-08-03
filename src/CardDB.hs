-- Functions to insert data into the database.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}

module CardDB where

import Control.Monad.IO.Class (liftIO)
import Database.Selda
import Database.Selda.SQLite

data Card = Card 
  { group :: Text
  , name  :: Text
  , img   :: Text
  , hp    :: Maybe Int }
  deriving Generic
instance SqlRow Card

-- DB file and image folder
file = "cards.sqlite"
imgs = "images/"

cards :: Table Card
cards = table "cards" [#name :- primary]

insertCard :: Text -> Text -> Text -> Maybe Int -> IO ()
insertCard group name img hp = withSQLite file $ do
  tryCreateTable cards
  result <- tryInsert cards
      [ Card group name (imgs <> img) hp ]
  liftIO $ print $ 
    if result
    then "Successfully created " <> show name <> " with " <> show hp <> " HP." <> " Image at: " <> show img 
    else "Error creating entry."

printCards :: IO ()
printCards = withSQLite file $ do
  tryCreateTable cards
  list <- query $ do
    card <- select cards
    return (card ! #group :*: card ! #name :*: card ! #hp :*: card ! #img)
  liftIO $ mapM_ print list 

removeCard :: Text -> IO ()
removeCard name = withSQLite file $ do 
  result <- deleteFrom cards (\card -> card ! #name .== literal name)
  case result of
    0 -> liftIO $ print "Could not remove entry."
    1 -> liftIO $ print $ "Successfully deleted " <> name <> "."
    _ -> liftIO $ print "This shouldn't happen."

-- Insert cards by group
hero    = "hero"
villain = "villain"
env     = "env"

insertHero    = insertCard hero
insertVillain = insertCard villain 
insertEnv     = insertCard env 

