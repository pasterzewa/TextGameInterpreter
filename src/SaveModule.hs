{-# OPTIONS -Wall #-}
{- |
Module responsible for save file system
-}
module SaveModule where

import GameEngine
import Text.ParserCombinators.Parsec
import InterpreterParsec

-- | Function starting the parsing of loaded Inventory.
loadParseInventory :: String -> Either ParseError Inventory
loadParseInventory input = parse parseLoadedInventory "(unknown)" input 

-- | Function starting the parsing of loaded Flags.
loadParseFlags :: String -> Either ParseError Flags 
loadParseFlags input = parse parseLoadedFlags "(unknown)" input 

-- | Function starting the parsing of loaded Scene id.
loadParseScene :: String -> Either ParseError Int
loadParseScene input = parse parseLoadedScene "(unknown)" input 

-- | Function parsing the loaded Inventory.
parseLoadedInventory :: CharParser () Inventory
parseLoadedInventory = try (string "Save file") >> spaces >> (Inventory <$ try (string "*Inventory*\n") <*> ((many (noneOf ",*\n")) `sepBy` (char ',')))

-- | Function parsing the loaded Flags.
parseLoadedFlags :: CharParser () Flags
parseLoadedFlags = try (string "Save file") >> spaces >> (string "*Inventory*") >> skipMany (noneOf "*") >> (Flags <$ try (string "*Flags*\n") <*> ((many (noneOf ",*\n")) `sepBy` (char ',')))

-- | Function parsing the loaded Scene id.
parseLoadedScene :: CharParser () Int
parseLoadedScene = try (string "Save file") >> spaces >> (string "*Inventory*") >> skipMany (noneOf "*") >> try (string "*Flags*") >> skipMany (noneOf "*") >> try (string "*Scene*") >> spaces >> (fmap toInt (many digit))

-- | Function creating a save file.
saveProgress :: Inventory -> Flags -> Int -> String -> IO()
saveProgress (Inventory inventory) (Flags flags) sceneKey saveFile = do 
    writeFile saveFile ("Save file\n*Inventory*\n" ++ (drop 1 (foldr joinWithComma "" inventory)) ++ "\n*Flags*\n" ++ (drop 1 (foldr joinWithComma "" flags)) ++ "\n*Scene*\n" ++ (show sceneKey))

-- | Function joining two strings separated with a comma, used when creating a save file.
joinWithComma :: String -> String -> String 
joinWithComma s1 s2 = s2 ++ "," ++ s1