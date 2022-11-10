{- |
Main module. Some functions are copyright Laurence Emms 2018. These have "LE" in their description. Some of those functions have been slightly changed.
-}
module Main where

import System.IO
import System.Exit
import Data.Char
import qualified Data.Text
import qualified Data.List.Split
import qualified Data.List
import qualified Data.Map
import Data.Either
import GameEngine
import Text.ParserCombinators.Parsec
import TextReflow
import Parser
import Lexer
import InterpreterParsec
import SaveModule

-- | LE. Function defining possible delimiters.
allDelimiters :: [Char] -- LE
allDelimiters = [' ', '\t']

-- | LE. Function defining column width of the text.
allColumnWidth :: Int -- LE (changed value)
allColumnWidth = 100

-- | LE. Function printing the intro of the program.
printIntro :: IO () -- LE
printIntro
    = reflowPutStrs allDelimiters 
                    allColumnWidth
                    ["Haskell Adventure Text Game Interpreter\n",
                     "\n\n"] >>
      hFlush stdout

-- | Function loading the inventory, when starting a new game.
loadInventory :: String -> Either ParseError Inventory
loadInventory game = mainParseInventory game 

-- | Function loading the flags, when starting a new game.
loadFlags :: String -> Either ParseError Flags
loadFlags game = mainParseFlags game 

-- | Function loading the inventory, when loading a save file.
loadLoadedInventory :: String -> Either ParseError Inventory
loadLoadedInventory save = loadParseInventory save

-- | Function loading the flags, when loading a save file.
loadLoadedFlags :: String -> Either ParseError Flags
loadLoadedFlags save = loadParseFlags save

-- | Function loading the id of the scene where the player saved the game.
loadLoadedScene :: String -> Either ParseError Int
loadLoadedScene save = loadParseScene save

-- | Function loading the game intro.
loadIntro :: String -> Either ParseError String
loadIntro game = mainParseIntro game 

-- | Function loading nouns present in the game.
loadNouns :: String -> Either ParseError [Token]
loadNouns game = mainParseNouns game

-- | Function loading verbs present in the game.
loadVerbs :: String -> Either ParseError [Token]
loadVerbs game = mainParseVerbs game

-- | Function loading all scenes present in the game.
loadAllScenes :: String -> Either ParseError [Scene]
loadAllScenes game = mainParseScenes game

-- | Function loading game's default scene.
loadDefaultScene :: String -> Either ParseError Scene
loadDefaultScene game = mainParseDefaultScene game

-- | Function loading game's end scenes ids.
loadEndScenes :: String -> Either ParseError [Int]
loadEndScenes game = mainParseEndScenes game

-- | Function loading game's all scenes ids.
loadAllSceneInds :: String -> Either ParseError [Int]
loadAllSceneInds game = mainParseScenesInds game

-- | Function joining scenes and their ids into tuples.
zipScenes :: [Int] -> [Scene] -> [(Int, Scene)]
zipScenes inds scenes = zip inds scenes

-- | Function mapping scenes and their ids.
mapScenes :: [Scene] -> [Int] -> Data.Map.Map Int Scene
mapScenes allScenes allSceneInds = Data.Map.fromList (zipScenes allSceneInds allScenes)

-- | Function returning the array instead of Either ParseError array.
getRight :: Either ParseError [a] -> [a] -- Nouns, Verbs, Scenes, Scene Indexes
getRight epa = fromRight [] epa

-- | Function returning the scene instead of Either ParseError Scene.
getDefaultScene :: Either ParseError Scene -> Scene
getDefaultScene epd = fromRight (Scene {ident=0, sceneDescr=(Description []), interactions=[]}) epd

-- | Function returning the Inventory instead of Either ParseError Inventory.
getInventory :: Either ParseError Inventory -> Inventory
getInventory epi = fromRight (Inventory []) epi

-- | Function returning the Flags instead of Either ParseError Flags.
getFlags :: Either ParseError Flags -> Flags
getFlags epf = fromRight (Flags []) epf

-- | LE. Function starting the adventure game.
adventure :: NarrativeGraph -> Maybe (Int, Inventory, Flags, [Token]) -> IO (Maybe (Int, Inventory, Flags, [Token])) -- LE (changed to use [Token])
adventure _ Nothing = hFlush stdout >> return Nothing
adventure narrativeGraph (Just (sceneKey, inventory, flags, allTokens))
    = printSceneDescription allDelimiters allColumnWidth narrativeGraph (Just (sceneKey, inventory, flags, allTokens)) >>=
      updateAdventure narrativeGraph

-- | LE. Functions responsible for updating the adventure game..
updateAdventure :: NarrativeGraph -> Maybe (Int, Inventory, Flags, [Token]) -> IO (Maybe (Int, Inventory, Flags, [Token])) -- LE (changed to use [Token])
updateAdventure _ Nothing = return Nothing
updateAdventure narrativeGraph (Just (sceneKey, inventory, flags, allTokens))
    = putStr "\n> " >>
      hFlush stdout >>
      printInvalidInteractions narrativeGraph sceneKey >>
      getLine >>=
      parseInput inventory flags allTokens sceneKey >>=
      (\state -> putStr "\n" >> hFlush stdout >> return state) >>=
      doAdventureLoop narrativeGraph sceneKey inventory flags allTokens

-- | LE. Function looping the adventure game. It goes back to function adventure.
doAdventureLoop :: NarrativeGraph -> Int -> Inventory -> Flags -> [Token] -> Maybe [Sentence] -> IO (Maybe (Int, Inventory, Flags, [Token])) -- LE (changed to use [Token])
doAdventureLoop _ _ _ _ _ Nothing = return Nothing -- End state of the game
doAdventureLoop narrativeGraph sceneKey inventory flags allTokens (Just []) = adventure narrativeGraph (Just (sceneKey, inventory, flags, allTokens)) --Failed to parse any sentences
doAdventureLoop narrativeGraph sceneKey inventory flags allTokens (Just sentences) = performInteraction allDelimiters allColumnWidth narrativeGraph sceneKey inventory flags allTokens sentences >>=
                                                                           adventure narrativeGraph --Perform the adventure loop

-- LE. Function responsible for parsing player's input
parseInput :: Inventory -> Flags -> [Token] -> Int -> String -> IO (Maybe [Sentence]) -- LE (changed to use [Token] and deleted a few options)
parseInput inventory flags allTokens sceneKey line
    | map Data.Char.toLower line == "help" = printHelp >> return (Just [])
    | map Data.Char.toLower line == "grammar" = putStrLn "All grammar:" >> printGrammar >> return (Just [])
    | map Data.Char.toLower line == "inventory" = putStrLn "All items in inventory:" >> printInventory inventory >> return (Just [])
    | map Data.Char.toLower line == "flags" = putStrLn "All currently set flags:" >> printFlags flags >> return (Just [])
    | map Data.Char.toLower line == "quit" = putStrLn "Thanks for playing!" >> hFlush stdout >> return Nothing
    | map Data.Char.toLower line == "save" = putStrLn "Please input save file name: " >> getLine >>= saveProgress inventory flags sceneKey >> return (Just [])
    | sentences == [] = putStr "I'm sorry, I don't understand what you said." >> hFlush stdout >> return (Just sentences)
    | otherwise = hFlush stdout >>
                  return (Just sentences)
        where inputWords = Data.List.Split.splitOneOf allDelimiters line
              sentenceTokenMatches = lexInput allTokens inputWords
              sentences = parseSentence sentenceTokenMatches

-- | LE. Function printing the current Inventory.
printInventory :: Inventory -> IO () -- LE
printInventory (Inventory []) = putStr "\n" >> hFlush stdout
printInventory (Inventory (object : remainingInventory))
    = reflowPutStr allDelimiters
                   allColumnWidth
                   (object ++ "\n") >>
      printInventory (Inventory remainingInventory) >> hFlush stdout

-- | LE. Function printing current active Flags.
printFlags :: Flags -> IO () -- LE
printFlags (Flags []) = putStr "\n" >> hFlush stdout
printFlags (Flags (flag : remainingFlags))
    = reflowPutStr allDelimiters
                   allColumnWidth
                   (flag ++ ".\n") >>
      printFlags (Flags remainingFlags) >> hFlush stdout

-- | LE. Function printing the grammar rules. Restricted to only Phrase and Simple Sentence.
printGrammar :: IO () -- LE (changed to only SimpleSentence and Phrase)
printGrammar
    = reflowPutStrs allDelimiters
                    allColumnWidth
                    ["Simple sentence: <Verb> <Noun>\n",
                     "Pharse: <Verb>\n"] >>
      hFlush stdout

-- | LE. Function printing possible commands.
printHelp :: IO () -- LE
printHelp
    = reflowPutStrs allDelimiters
                    allColumnWidth
                    ["The following commands are available:\n",
                     "Inventory - Print all current inventory items.\n",
                     "Help - Print help text.\n",
                     "Grammar - Print available grammar.\n",
                     "Flags - Print all current flags. Warning, this contains spoilers!\n",
                     "Quit - Exit the  game.\n",
                     "Save - Save current game progress.\n",
                     "--------------------"] >>
      hFlush stdout

-- | Starting function of the program.
main :: IO ()
main = do
      printIntro 
      putStr "Please input game file name: "
      fileName <- getLine
      s <- readFile fileName
      putStr "Please input save file name, if you have one: "
      saveName <- getLine
      if all isSpace saveName
            then do
                  putStrLn "You will be starting the game from the beggining" 
                  putStrLn "\n\n\n"
                  putStrLn (fromRight "" (loadIntro s))
                  adventure  (makeNarrativeGraph  (mapScenes (getRight (loadAllScenes s)) (getRight (loadAllSceneInds s)))  
                        (getRight (loadEndScenes s)) (getDefaultScene (loadDefaultScene s)))
                        (Just (1, (getInventory (loadInventory s)), (getFlags (loadFlags s)),
                        (getRight (loadVerbs s))++(getRight (loadNouns s))))
                  putStrLn ("Till next time!\n")
            else do 
                  t <- readFile saveName 
                  putStrLn "\n\n\n"
                  adventure  (makeNarrativeGraph  (mapScenes (getRight (loadAllScenes s)) (getRight (loadAllSceneInds s)))  
                        (getRight (loadEndScenes s)) (getDefaultScene (loadDefaultScene s)))
                        (Just ((fromRight 1 (loadLoadedScene t)), (getInventory (loadLoadedInventory t)), (getFlags (loadLoadedFlags t)),
                        (getRight (loadVerbs s))++(getRight (loadNouns s))))
                  putStrLn ("Till next time!\n")