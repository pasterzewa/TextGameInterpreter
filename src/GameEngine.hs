{-# OPTIONS -Wall #-}
{- | Copyright Laurence Emms 2018.
Module for representing a narrative graph in a text adventure with several changes compared to the original version:
scenes are only identified by Int, CAnd and COr operate on arrays of conditions, not only two elements, 
a few data types name were changed, [Token] was added in several functions and some smaller changes were made to
ensure patterns are complete and no unnesessary things are defined.
-}
module GameEngine where

import Parser
import Lexer
import TextReflow
import System.IO 
import Data.List
import qualified Data.Map 

{- | Flags type contains what benchmarks have been reached, what decisions have been made etc. Example of Flags type:

__Examples:__

@
flags = Flags ["game started", "door open"]
@
-}
data Flags = Flags [String] deriving (Show, Eq)

{- | Inventory type defines what objects, defined as strings, the player has at any given moment. Example of Inventory type:

__Examples:__

@
inventory = Inventory ["key", "energy drink", "shoelace"]
@
-}
data Inventory = Inventory [String] deriving (Show, Eq)

{- | StateChange type is used to define how the state of our game is supposed to change. Examples of StateChange type:

__Examples:__

@
addShoe = AddToInventory "shoe"
removeDrink = RemoveFromInventory "energy drink"
befriendedCat = SetFlag "cat friendly"
zombified = RemoveFlag "alive"
changeScene = ChangeScene 3
@
-}
data StateChange = AddToInventory String -- ^ Constructor for changing (adding) the state of inventory
                    | RemoveFromInventory String -- ^ Constructor for changing (removing) the state of inventory
                    | SetFlag String -- ^ Constructor for changing (adding) the state of flags
                    | RemoveFlag String -- ^ Constructor for changing (removing) the state of flags
                    | ChangeScene Int -- ^ Constructor for changing the scene
                        deriving (Show , Eq)

{- | Condition type is used to check if specific criterions are met. Examples of Condition type:

__Examples:__

@
notBarefoot = InInventory "shoe"
barefoot = CNot InInventory "shoe"
readyForPicnic = CAnd [InInventory "picninc basket", FlagSet "picnic basket full", FlagSet "sunny", SceneIs 5]
@
-}
data Condition = InInventory String -- ^ Constructor for checking if object is in inventory
                 | FlagSet String -- ^ Constructor for checking if specific flag is set
                 | SceneIs Int -- ^ Constructor for checking if it's the proper scene
                 | CTrue  -- ^ Always true condition
                 | CFalse -- ^ Always false condition
                 | CNot Condition -- ^ Constructor for negating a condition
                 | COr [Condition] -- ^ Constructor using or operator on multiple conditions
                 | CAnd [Condition] -- ^ Constructor using and operator on multiple conditions
                    deriving (Show, Eq)

{- | Description type is a conditional description. Example of Description type:

__Examples:__

@
descriptionOfWeather = Description [(FlagSet "sunny", "The weather is sunny and beautiful", []), (CNot FlagSet "sunny", "The weather is awful and you regret coming to the park.", [SetFlag "bad mood"])]
@
-}
data Description = Description [(Condition, String, [StateChange])] deriving (Show, Eq)

{- | Action type describes the consequences of specific actions.
-}
data Action = Action {cond :: Condition, actionDescr :: String, changes :: [StateChange]} deriving (Show, Eq)

{- | Interaction type defines what sentence causes what consequences.
-}
data Interaction = Interaction {sntnc :: Sentence, actions :: [Action]} deriving (Show, Eq)

{- | Scene type holds all descriptions and interactions possible for this specific scene. Scenes have unique numbers (Int).
-}
data Scene = Scene {ident :: Int, sceneDescr :: Description, interactions :: [Interaction]} deriving (Show, Eq)

{- | NarrativeGraph type hold all scenes and their mapping with their ids, as well as information which scenes signal the end of the game
and the default scene, used when no other matches.
-}
data NarrativeGraph = NarrativeGraph {nodes :: Data.Map.Map Int Scene, 
                    endScenes :: [Int], graphDefaultScene :: Scene} deriving (Show, Eq)

-- | Takes a list of scenes and returns a starting index and a NarrativeGraph
makeNarrativeGraph :: Data.Map.Map Int Scene -> [Int] -> Scene -> NarrativeGraph
makeNarrativeGraph scenes scenesEndScenes defaultScene
    = NarrativeGraph {nodes = scenes,
                      endScenes = scenesEndScenes,
                      graphDefaultScene = defaultScene}

-- | Evaluates specific conditions
evaluateCondition :: Condition -> Int -> Inventory -> Flags -> [Token] -> Bool
evaluateCondition CTrue _ _ _ _ = True
evaluateCondition CFalse _ _ _ _ = False
evaluateCondition (FlagSet flag) _ _ (Flags flags) _ = flag `elem` flags
evaluateCondition (SceneIs scene) currentScene _ _ _ = scene == currentScene
evaluateCondition (InInventory object) _ (Inventory inventory) _ _ = object `elem` inventory
evaluateCondition (CNot condition) scene inventory flags allTokens = not (evaluateCondition condition scene inventory flags allTokens)
evaluateCondition (COr condTab) scene inventory flags allTokens = foldr (||) False (map (\x -> evaluateCondition x scene inventory flags allTokens) condTab) 
evaluateCondition (CAnd condTab) scene inventory flags allTokens = foldr (&&) True (map (\x -> evaluateCondition x scene inventory flags allTokens) condTab) 


-- | Print a conditional description by evaluating which conditions are true, concatenating the description, and printing it with reflowPutStrs
printConditionalDescription :: [Char] -> Int -> [Int] -> Description -> [String] -> Maybe (Int, Inventory, Flags, [Token]) -> IO (Maybe (Int, Inventory, Flags, [Token]))
printConditionalDescription delimiters columnWidth _ (Description []) linesToPrint Nothing
    = reflowPutStrs delimiters columnWidth (reverse linesToPrint) >> putStr "\n" >> return Nothing --Game reached an end state --reflowPutStrs delimiters columnWidth
printConditionalDescription delimiters columnWidth _ (Description []) linesToPrint (Just (sceneKey, inventory, flags, allTokens))
    = reflowPutStrs delimiters columnWidth (reverse linesToPrint) >> putStr "\n" >> hFlush stdout >> return (Just (sceneKey, inventory, flags, allTokens)) --No more descriptions to print
printConditionalDescription delimiters columnWidth _ (Description ((_, _, _) : _)) linesToPrint Nothing
    = reflowPutStrs delimiters columnWidth (reverse linesToPrint) >> putStr "\n" >> return Nothing --Game reached an end state
printConditionalDescription delimiters columnWidth endSceness
                            (Description ((condition, subDescription, stateChanges) : remainingDescriptions)) linesToPrint (Just (sceneKey, inventory, flags, allTokens))
    | evaluateCondition condition sceneKey inventory flags allTokens =
          stateChange (Data.List.find (\x -> case x of
                                             (ChangeScene _) -> True
                                             _ -> False) stateChanges)
                       endSceness
                       stateChanges
                       (Just (sceneKey, inventory, flags, allTokens)) >>= --This conditional description passed all of the preconditions, check whether we need to transition to a new state
          printConditionalDescription delimiters columnWidth endSceness (Description remainingDescriptions) ((subDescription ++ " ") : linesToPrint) --Condition is true, add sub-description to print
    | otherwise
        = printConditionalDescription delimiters columnWidth endSceness (Description remainingDescriptions) linesToPrint (Just (sceneKey, inventory, flags, allTokens))

-- | Prints scene's description
printSceneDescription :: [Char] -> Int -> NarrativeGraph -> Maybe (Int, Inventory, Flags, [Token]) -> IO (Maybe (Int, Inventory, Flags, [Token]))
printSceneDescription _ _ (NarrativeGraph {nodes = _, endScenes = _}) Nothing
    = return Nothing
printSceneDescription delimiters columnWidth (NarrativeGraph {nodes = graphNodes, endScenes = graphEndScenes}) (Just (sceneKey, inventory, flags, allTokens))
    = let scene = Data.Map.lookup sceneKey graphNodes
      in case scene of
         Nothing -> putStrLn ((show sceneKey) ++ " is not a valid scene") >> return Nothing
         Just (Scene {sceneDescr = thisSceneDescription,
                      interactions = _}) -> printConditionalDescription delimiters columnWidth graphEndScenes thisSceneDescription [] (Just (sceneKey, inventory, flags, allTokens))

-- | Updates the Flags according to the passed StateChanges
updateFlags :: Flags -> [StateChange] -> Flags
updateFlags (Flags flags) [] = Flags flags
updateFlags (Flags flags) ((RemoveFlag flag) : remainingChanges) = updateFlags (Flags (filter (\x -> x /= flag) flags)) remainingChanges
updateFlags (Flags flags) ((SetFlag flag) : remainingChanges)
    | flag `elem` flags = updateFlags (Flags flags) remainingChanges
    | otherwise = updateFlags (Flags (flag : flags)) remainingChanges
updateFlags (Flags flags) (_ : remainingChanges) = updateFlags (Flags flags) remainingChanges

-- | Updates the Inventory according to the passed StateChanges
updateInventory :: Inventory -> [StateChange] -> Inventory
updateInventory (Inventory inventory) [] = Inventory inventory
updateInventory (Inventory inventory) ((RemoveFromInventory object) : remainingChanges) = updateInventory (Inventory (filter (\x -> x /= object) inventory)) remainingChanges
updateInventory (Inventory inventory) ((AddToInventory object) : remainingChanges)
    | object `elem` inventory = updateInventory (Inventory inventory) remainingChanges
    | otherwise = updateInventory (Inventory (object : inventory)) remainingChanges
updateInventory (Inventory inventory) (_ : remainingChanges) = updateInventory (Inventory inventory) remainingChanges

-- | Updates all states (Inventory, Flags, Scene)
stateChange :: Maybe StateChange -> [Int] -> [StateChange] -> Maybe (Int, Inventory, Flags, [Token]) -> IO (Maybe (Int, Inventory, Flags, [Token]))
stateChange Nothing _ _ Nothing
    = return Nothing
stateChange _ _ _ Nothing 
    = return Nothing
stateChange Nothing _  stateChanges (Just (sceneKey, inventory, flags, allTokens))
    = return (Just (sceneKey,
                    updateInventory inventory stateChanges,
                    updateFlags flags stateChanges,
                    allTokens )) --If there is no scene transition, return to the current scene with updated inventory and flags
stateChange (Just (ChangeScene nextScene)) endSceness stateChanges (Just (_, inventory, flags, allTokens)) 
    = if nextScene `elem` endSceness
      then getChar >> return Nothing --This is an end state for the game
      else return (Just (nextScene,
                         updateInventory inventory stateChanges,
                         updateFlags flags stateChanges,
                         allTokens )) --Transition to the next scene with updated inventory and flags
stateChange _ _ _ _ = return Nothing

-- | UpdateGameState takes an interaction description, fail string, next scene index, end scene index, current scene index, inventory, and flags.
-- | UpdateGameState Scene transition evaluates to the next state of the game.
updateGameState :: [Char] -> Int -> [Int] -> Int -> Inventory -> Flags -> [Token]-> Action -> IO (Maybe (Int, Inventory, Flags, [Token]))
updateGameState delimiters columnWidth endSceness currentSceneKey inventory flags allTokens (Action { cond = thisCondition,
                                                                                                    actionDescr = thisConditionalDescription,
                                                                                                    changes = thisStateChanges})
    = printConditionalDescription delimiters columnWidth endSceness (Description [(thisCondition, thisConditionalDescription, [])]) [] (Just (currentSceneKey, inventory, flags, allTokens)) >>=
      stateChange (Data.List.find (\x -> case x of
                                         (ChangeScene _) -> True
                                         _ -> False) thisStateChanges)
                   endSceness
                   thisStateChanges --This conditional action passed all of the preconditions, check whether we need to transition to a new scene

-- | Perform the interaction and return a tuple of (new scene index, new inventory, new flags)
performConditionalActions :: [Char] -> Int -> Int -> [Int] -> Inventory -> Flags -> [Token] -> Maybe Interaction -> Maybe Interaction -> IO (Maybe (Int, Inventory, Flags, [Token]))
performConditionalActions delimiters
                          columnWidth
                          currentSceneKey
                          endSceness
                          inventory
                          flags
                          allTokens
                          (Just (Interaction {sntnc = _,
                                              actions = []}))
                          defaultSceneInteractions --There are no remaining conditional actions for the current scene
    = performConditionalActions delimiters columnWidth currentSceneKey endSceness inventory flags allTokens Nothing defaultSceneInteractions --All current scene conditional actions were exhausted, try default scene interactions
performConditionalActions delimiters
                          columnWidth
                          currentSceneKey
                          endSceness
                          inventory
                          flags
                          allTokens
                          (Just (Interaction {sntnc = thisSentences,
                                              actions = (conditionalAction@(Action {cond = thisCondition}) : remainingConditionalActions)}))
                          defaultSceneInteractions -- Ignore default scene interactions if there are still current scene interactions
    | evaluateCondition thisCondition currentSceneKey inventory flags allTokens = updateGameState delimiters columnWidth endSceness currentSceneKey inventory flags allTokens conditionalAction --The condition for the action passed, update the game state
    | otherwise = performConditionalActions delimiters columnWidth currentSceneKey endSceness inventory flags allTokens
                                            (Just (Interaction {sntnc = thisSentences,
                                                                actions = remainingConditionalActions})) defaultSceneInteractions --The condition for the action failed, attempt other actions
performConditionalActions delimiters
                          columnWidth
                          currentSceneKey
                          endSceness
                          inventory
                          flags
                          allTokens
                          Nothing --The current scene failed to have any interactions
                          (Just (Interaction {sntnc = _,
                                              actions = []}))
    = performConditionalActions delimiters columnWidth currentSceneKey endSceness inventory flags allTokens Nothing Nothing --All possible conditional actions are exhausted
performConditionalActions delimiters
                          columnWidth
                          currentSceneKey
                          endSceness
                          inventory
                          flags
                          allTokens
                          Nothing --The current scene failed to have any interactions
                          (Just (Interaction {sntnc = thisSentences,
                                              actions = (conditionalAction@(Action {cond = thisCondition}) : remainingConditionalActions)}))
    | evaluateCondition thisCondition currentSceneKey inventory flags allTokens = updateGameState delimiters columnWidth endSceness currentSceneKey inventory flags allTokens conditionalAction --The condition for the action passed, update the game state
    | otherwise = performConditionalActions delimiters columnWidth currentSceneKey endSceness inventory flags allTokens Nothing
                                            (Just (Interaction {sntnc = thisSentences,
                                                                actions = remainingConditionalActions})) --The condition for the action failed, attempt other actions
performConditionalActions _ _ currentSceneKey _ inventory flags allTokens Nothing Nothing
    = putStr "That does nothing." >>
      hFlush stdout >>
      return (Just (currentSceneKey, inventory, flags, allTokens)) --If there are no valid interactions actions but the sentence was valid, just return to the current state

-- | Checks if the given sentence is part of the given interaction
matchInteraction :: (Interaction, Sentence) -> Bool
matchInteraction ((Interaction {sntnc = thisSentences}), sentence)
     | sentence == thisSentences = True
     | otherwise = False

-- | Finds the interaction matching the given sentences
findInteraction :: [Interaction] -> [Sentence] -> Maybe Interaction
findInteraction interactionss sentences = (find matchInteraction ((\x -> (\y -> (x, y))) <$> interactionss <*> sentences)) >>= (\(x, _) -> Just x)

-- | Finds the proper interaction and performs associated actions
filterInteraction :: [Char] -> Int -> Scene -> Scene -> Int -> [Int] -> Inventory -> Flags -> [Token] -> [Sentence] -> IO (Maybe (Int, Inventory, Flags, [Token]))
filterInteraction delimiters
                  columnWidth
                  (Scene {sceneDescr = _,
                          interactions = thisSceneInteractions})
                  (Scene {sceneDescr = _,
                          interactions = defaultSceneInteractions})
                  currentSceneKey
                  endSceness
                  inventory
                  flags
                  allTokens
                  sentences
    = performConditionalActions delimiters columnWidth currentSceneKey endSceness inventory flags allTokens interaction defaultInteraction
        where interaction = findInteraction thisSceneInteractions sentences
              defaultInteraction = findInteraction defaultSceneInteractions sentences

-- | Checks if given interactions contain invalid ones
hasInvalidInteractions :: [Interaction] -> Maybe Interaction
hasInvalidInteractions [] = Nothing
hasInvalidInteractions (interaction@(Interaction {sntnc = thisSentences}) : remainingInteractions)
    | NullSentence == thisSentences = Just interaction
    | otherwise = hasInvalidInteractions remainingInteractions

-- | Prints invalid interactions
printInvalidInteractions :: NarrativeGraph -> Int -> IO ()
printInvalidInteractions (NarrativeGraph {nodes = graphNodes}) sceneKey
    = let scene = Data.Map.lookup sceneKey graphNodes 
      in case scene of
          Nothing -> putStrLn ((show sceneKey) ++ " is not a valid scene") >> return ()
          Just (Scene {sceneDescr = _, interactions = sceneInteractions})
              -> case hasInvalidInteractions sceneInteractions of
                     Nothing -> return ()
                     Just interaction@(Interaction {sntnc = _}) -> putStrLn ("Invalid interaction: " ++  (show interaction))

-- | Performs the given interaction
performInteraction :: [Char] -> Int -> NarrativeGraph -> Int -> Inventory -> Flags -> [Token] -> [Sentence] -> IO (Maybe (Int, Inventory, Flags, [Token]))
performInteraction _ _ _ sceneKey inventory flags allTokens []
    = putStrLn "Please enter a command." >>
      hFlush stdout >>
      return (Just (sceneKey, inventory, flags, allTokens)) --If there are no valid sentences, just continue.
performInteraction delimiters columnWidth (NarrativeGraph {nodes = graphNodes, endScenes = graphEndScenes, graphDefaultScene = thisDefaultScene}) sceneKey inventory flags allTokens sentences
    = let scene = Data.Map.lookup sceneKey graphNodes
      in case scene of
             Nothing -> hFlush stdout >> putStrLn ((show sceneKey) ++ " is not a valid scene") >> return Nothing
             Just currentScene -> hFlush stdout >>
                                  filterInteraction delimiters columnWidth currentScene thisDefaultScene sceneKey graphEndScenes inventory flags allTokens sentences
