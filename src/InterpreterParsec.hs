{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{- |
Module responsible for parsing the game file.
-}
module InterpreterParsec where

import Text.ParserCombinators.Parsec
import GameEngine
import Parser
import Lexer

-- | Function to parse String to Int.
toInt :: String -> Int 
toInt x = read x :: Int

-- | Function starting parsing of Inventory.
mainParseInventory :: String -> Either ParseError Inventory
mainParseInventory input = parse parseInventory "(unknown)" input 

-- | Function starting parsing of Flags.
mainParseFlags :: String -> Either ParseError Flags 
mainParseFlags input = parse parseFlags "(unknown)" input 

-- | Function starting parsing of Game Intro.
mainParseIntro :: String -> Either ParseError String 
mainParseIntro input = parse parseGameIntro "(unknown)" input 

-- | Function starting parsing of Nouns.
mainParseNouns :: String -> Either ParseError [Token] 
mainParseNouns input = parse parseNouns "(unknown)" input 

-- | Function starting parsing of Verbs.
mainParseVerbs :: String -> Either ParseError [Token] 
mainParseVerbs input = parse parseVerbs "(unknown)" input 

-- | Function starting parsing of all Scenes.
mainParseScenes :: String -> Either ParseError [Scene]
mainParseScenes input = parse parseScenes "unknown" input

-- | Function starting parsing of Default Scene.
mainParseDefaultScene :: String -> Either ParseError Scene
mainParseDefaultScene input = parse parseDefaultScene "unknown" input

-- | Function starting parsing of ids of End Scenes.
mainParseEndScenes :: String -> Either ParseError [Int]
mainParseEndScenes input = parse parseEndScenes "unknown" input

-- | Function starting parsing of all scene ids.
mainParseScenesInds :: String -> Either ParseError [Int]
mainParseScenesInds input = parse parseAllScenes "unknown" input

-- | Function parsing a Condition.
parseCondition :: CharParser () Condition
parseCondition = FlagSet <$ string "FlagSet " <*> many (noneOf ";,\n") <|> 
    InInventory <$ string "InInventory " <*> many (noneOf ";,\n") <|>
    CNot <$ try (string "CNot ") <*> try parseCondition <|>
    CTrue <$ try (string "CTrue") <|>
    CFalse <$ try (string "CFalse") <|>
    SceneIs <$ string "SceneIs " <*> fmap toInt (many digit) <|>
    CAnd <$ try (string "CAnd ") <*> parseCondition `sepBy` (char ',') <|>
    COr <$ try (string "COr ") <*> parseCondition `sepBy` (char ',')

-- | Function parsing a StateChange.
parseStateChange :: CharParser () StateChange
parseStateChange = AddToInventory <$ try (string "AddToInventory ") <*> many (noneOf "<,;\n") <|>
    RemoveFromInventory <$ try (string "RemoveFromInventory ") <*> many (noneOf "<,;\n") <|>
    SetFlag <$ try (string "SetFlag ") <*> many (noneOf "<,;\n") <|>
    RemoveFlag <$ try (string "RemoveFlag ") <*> many (noneOf "<,;\n") <|>
    ChangeScene <$ try (string "ChangeScene ") <*> fmap toInt (many digit)

-- | Function parsing multiple StateChanges.
parseChanges :: CharParser () [StateChange]
parseChanges = parseStateChange `sepBy` (char ',')

-- | Function parsing a pair of Condition and String and add empty StateChanges.
parseCondStr :: CharParser () (Condition, String, [StateChange])
parseCondStr = do
    condd <- try parseCondition 
    _ <- char ';'
    _ <- char ' '
    str <- try (many (noneOf "<;"))
    return (condd, str, [])

-- | Function parsing a list of Condition, String pairs.
listCondStr :: CharParser () [(Condition, String, [StateChange])]
listCondStr = parseCondStr `endBy` (string "<\n")

-- | Function parsing a Description.
parseDescription :: CharParser () Description
parseDescription = Description <$ string "Description:\n" <*> try listCondStr

-- | Function parsing an Action.
action :: CharParser () Action
action = do 
    condd <- try parseCondition
    _ <- char ';'
    spaces
    descr <- try (many (noneOf "<;"))
    _ <- char ';'
    spaces
    changess <- try parseChanges
    return Action {cond=condd, actionDescr=descr, changes=changess}

-- | Function parsing multiple Actions.
parseActions :: CharParser () [Action]
parseActions = do
    _ <- try (string "Actions:\n")
    list <- action `endBy` (string "<\n")
    return list

-- | Function parsing a Sentence.
parseSntnc :: CharParser () Sentence
parseSntnc = try (NullSentence <$ char ' ') <|> try parsePhrase <|> try parseSimpleSentence 

-- | Function parsing a Phrase.
parsePhrase :: CharParser () Sentence
parsePhrase = do
    tok <- try parseTokenV
    _ <- try  (char ';')
    return (Phrase tok)

-- | Function parsing a Simple Sentence.
parseSimpleSentence :: CharParser () Sentence
parseSimpleSentence = do
    tok1 <- try parseTokenV 
    _ <- try (char '|')
    tok2 <- try parseTokenN
    _ <- try (char ';')
    return (SimpleSentence tok1 tok2)

-- | Function parsing a Token that is a Token Verb.
parseTokenV :: CharParser () Token
parseTokenV = do
    tok <- try (many (noneOf "(|;,\n"))
    _ <- try (char '(')
    toks <- try ((many (noneOf "()|;,\n")) `sepBy` (char ','))
    _ <- try (char ')')
    return (TokenVerb tok toks)

-- | Function parsing a Token that is a Token Noun.
parseTokenN :: CharParser () Token
parseTokenN = do
    tok <- try (many (noneOf ";,\n "))
    return (TokenNoun tok [tok])

-- | Function parsing an Interaction.
parseInteraction :: CharParser () Interaction 
parseInteraction = do
    _ <- try (char '[')
    spaces
    _ <- try (string "Sentence: ")
    stn <- try parseSntnc
    spaces 
    actns <- try parseActions
    spaces
    _ <- try (char ']')
    spaces
    return Interaction {sntnc=stn, actions=actns}

-- | Function parsing multiple Interactions that start with "Interactions:" header.
parseAllInteractions :: CharParser () [Interaction]
parseAllInteractions = do
    _ <- try (string "Interactions:\n")
    list <- (try parseInteractions <|> ((lookAhead (char '}')) >> return []))
    return list

-- | Function parsing multiple Interactions.
parseInteractions :: CharParser () [Interaction]
parseInteractions = do
    int1 <- try parseInteraction
    rest <- remainingInteraction
    return (int1 : rest)

-- | Function helping with parsing multiple Interactions.
remainingInteraction :: CharParser () [Interaction]
remainingInteraction = (lookAhead (char '[') >> parseInteractions) <|> (return [])

-- | Function parsing a pair of Description and an array of Interactions.
parseDescrInter :: CharParser () (Description, [Interaction])
parseDescrInter = do 
    dscr <- try parseDescription
    spaces 
    inter <- try parseAllInteractions
    return (dscr, inter)

-- | Function parsing a Scene.
parseScene :: CharParser () Scene
parseScene = do
    _ <- try (string ("*Scene"))
    idnt <- try (fmap toInt (many digit))
    _ <- try (char '*')
    spaces
    _ <- try (char '{')
    spaces
    descr <- try parseDescription
    spaces
    inter <- try parseAllInteractions
    spaces
    return Scene {ident=idnt, sceneDescr=descr, interactions=inter}

-- | Function finding and then parsing multiple Scenes.
parseScenes :: CharParser () [Scene]
parseScenes = do 
    _ <- try (string "*GameIntro*")
    skipMany (noneOf "*")
    _ <- try (string "*Verbs*")
    skipMany (noneOf "*")
    _ <- try (string "*Nouns*")
    skipMany (noneOf "*")
    _ <- try (string "*Inventory*")
    skipMany (noneOf "*")
    _ <- try (string "*Flags*")
    skipMany (noneOf "*")
    _ <- try (string "*AllScenes*")
    skipMany (noneOf "*")
    _ <- try (string "*EndScenes*")
    skipMany (noneOf "*")
    _ <- try (string "*DefaultScene*")
    skipMany (noneOf "*")
    scenes <- try (parseScene `endBy` (string "}\n"))
    return scenes

-- | Function finding and then parsing a Default Scene.
parseDefaultScene :: CharParser () Scene
parseDefaultScene = do 
    _ <- try (string "*GameIntro*")
    skipMany (noneOf "*")
    _ <- try (string "*Verbs*")
    skipMany (noneOf "*")
    _ <- try (string "*Nouns*")
    skipMany (noneOf "*")
    _ <- try (string "*Inventory*")
    skipMany (noneOf "*")
    _ <- try (string "*Flags*")
    skipMany (noneOf "*")
    _ <- try (string "*AllScenes*")
    skipMany (noneOf "*")
    _ <- try (string "*EndScenes*")
    skipMany (noneOf "*")
    _ <- try (string "*DefaultScene*")
    spaces
    _ <- try (char '{')
    spaces
    descr <- try parseDescription
    spaces
    inter <- try parseAllInteractions
    spaces
    _ <- try (char '}')
    return Scene {ident=0, sceneDescr=descr, interactions=inter}

-- | Function finding and then parsing the array of end scene ids.
parseEndScenes :: CharParser () [Int]
parseEndScenes = do
    _ <- try (string "*GameIntro*")
    skipMany (noneOf "*")
    _ <- try (string "*Verbs*")
    skipMany (noneOf "*")
    _ <- try (string "*Nouns*")
    skipMany (noneOf "*")
    _ <- try (string "*Inventory*")
    skipMany (noneOf "*")
    _ <- try (string "*Flags*")
    skipMany (noneOf "*")
    _ <- try (string "*AllScenes*")
    skipMany (noneOf "*")
    _ <- try (string "*EndScenes*")
    spaces
    ends <- (fmap toInt (many digit)) `sepBy` (char ',')
    return ends

-- | Function finding and then parsing the array of all scene ids.
parseAllScenes :: CharParser () [Int]
parseAllScenes = do
    _ <- try (string "*GameIntro*")
    skipMany (noneOf "*")
    _ <- try (string "*Verbs*")
    skipMany (noneOf "*")
    _ <- try (string "*Nouns*")
    skipMany (noneOf "*")
    _ <- try (string "*Inventory*")
    skipMany (noneOf "*")
    _ <- try (string "*Flags*")
    skipMany (noneOf "*")
    _ <- try (string "*AllScenes*")
    spaces
    inds <- (fmap toInt (many digit)) `sepBy` (char ',')
    return inds

-- | Function parsing an Inventory.
parseStartingInventory :: CharParser () Inventory
parseStartingInventory = Inventory <$ (try (string "*Inventory* ") <|> try (string "*Inventory*\n")) <*> (many (noneOf "*,;\n")) `sepBy` (char ',')

-- | Function finding and then parsing an Inventory.
parseInventory :: CharParser () Inventory
parseInventory = do
    _ <- try (string "*GameIntro*")
    skipMany (noneOf "*")
    _ <- try (string "*Verbs*")
    skipMany (noneOf "*")
    _ <- try (string "*Nouns*")
    skipMany (noneOf "*")
    inv <- parseStartingInventory
    return inv

-- | Function parsing Flags.
parseStartingFlags :: CharParser () Flags
parseStartingFlags = Flags <$ (try (string "*Flags* ") <|> try (string "*Flags*\n")) <*> (many (noneOf "*,;\n")) `sepBy` (char ',')

-- | Function finding and then parsing Flags.
parseFlags :: CharParser () Flags
parseFlags = do
    _ <- try (string "*GameIntro*")
    skipMany (noneOf "*")
    _ <- try (string "*Verbs*")
    skipMany (noneOf "*")
    _ <- try (string "*Nouns*")
    skipMany (noneOf "*")
    _ <- try (string "*Inventory*")
    skipMany (noneOf "*")
    flg <- parseStartingFlags
    return flg

-- | Function finding and then parsing Game Intro (which is a String).
parseGameIntro :: CharParser () String
parseGameIntro = do
    spaces 
    _ <- try (string "*GameIntro*")
    spaces 
    result <- (many (noneOf "*"))
    return result

-- | Function parsing a Token that is a TokenNoun, used when parsing all available nouns.
parseTokenNoun :: CharParser () Token
parseTokenNoun = do 
    n <- many (noneOf ",\n")
    return (TokenNoun n [n])

-- | Function finding and then parsing multiple Tokens that are TokenNouns.
parseNouns :: CharParser () [Token]
parseNouns = do 
    _ <- try (string "*GameIntro*")
    skipMany (noneOf "*")
    _ <- try (string "*Verbs*")
    skipMany (noneOf "*")
    _ <- try (string "*Nouns*")
    spaces
    nouns <- parseTokenNoun `sepBy` (char ',')
    return nouns

-- | Function parsing a Token that is a TokenVerb, used when parsing all available verbs.
parseTokenVerb :: CharParser () Token
parseTokenVerb = do 
    v <- many (noneOf "(*\n")
    _ <- char '('
    alts <- ((many (noneOf ",\n)")) `sepBy` (char ','))
    _ <- char ')'
    return (TokenVerb v alts)

-- | Function finding and then parsing multiple Tokens that are TokenVerbs.
parseVerbs :: CharParser () [Token]
parseVerbs = do 
    _ <- try (string "*GameIntro*")
    skipMany (noneOf "*")
    _ <- try (string "*Verbs*")
    spaces
    verbs <- try (parseTokenVerb `endBy` (char '\n'))
    return verbs