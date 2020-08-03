-- Helper to create a structured database of cards with metadata and images.
-- Every card needs an image. Images should be located in a subfolder called images.

module Main where

import Data.Text (pack)
import System.Directory (doesFileExist)
import Text.Read (readMaybe)
import Control.Monad.IO.Class (liftIO)

import CardDB

main :: IO ()
main = do
  putStrLn "(1) Add hero | (2) Add villain | (3) Add environment | (4) Remove card | (5) List cards"
  option <- getLine
  case readMaybe option :: Maybe Int of
    Just 1 -> do 
      putStrLn "Enter hero name: "
      name <- getLine
      putStrLn "Enter hero HP: "
      hp <- getLine
      putStrLn "Enter image path: "
      img <- getLine
      insertHero (pack name) (pack img) (Just (read hp :: Int))
      main
    Just 2 -> do 
      putStrLn "Enter villain name: "
      name <- getLine
      putStrLn "Enter villain HP: "
      hp <- getLine
      putStrLn "Enter image path: "
      img <- getLine
      insertVillain (pack name) (pack img) (Just (read hp :: Int)) 
      main
    Just 3 -> do 
      putStrLn "Enter environment name: "
      name <- getLine
      putStrLn "Enter image path: "
      img <- getLine
      insertEnv (pack name) (pack img) Nothing
      main
    Just 4 -> do
      putStrLn "Enter card name to be deleted: "
      name <- getLine
      removeCard (pack name)
      main
    Just 5 -> printCards >> main
    _ -> putStrLn "Invalid option." >> main
