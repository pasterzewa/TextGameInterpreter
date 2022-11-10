{-# OPTIONS -Wall #-}
{- | Copyright Laurence Emms 2018.
Module responsible for correct "tokenization" and matching of input. Simplified compared to original version by Emms.
-}
module Lexer where


import qualified Data.Char

{- | Example of Token type.

__Examples:__

@
verb = TokenVerb "run" ["run","dash","race"]
noun = TokenNoun "gem" ["gem","jewel","shiny stone"]
@
-}
data Token = TokenVerb String [String] -- ^ First constructor
             | TokenNoun String [String] -- ^ Second constructor
                deriving (Show, Eq)   

{- | Example of TokenMatch type.

__Examples:__

@
match = TokenMatch "verb" [TokenVerb "verb" ["verb","synonym1","synonym2"]]
@
-}
data TokenMatch = TokenMatch String [Token]  -- ^ Constructor
                    deriving (Show, Eq) 

-- | This is a join function, joining two TokenMatches
join :: Maybe TokenMatch -> Maybe TokenMatch -> Maybe TokenMatch
join Nothing Nothing = Nothing
join (Just a) Nothing = Just a
join Nothing (Just b) = Just b
join (Just (TokenMatch wordA tokensA)) (Just (TokenMatch wordB tokensB))
    | wordA == wordB = Just (TokenMatch wordA (tokensA ++ tokensB))
    | otherwise = Nothing

-- | This is a tokenize function, responisble fot checking if the string is acceptable version of the token
tokenize :: String -> Token -> Maybe TokenMatch 
tokenize "" _ = Nothing 
tokenize word token@(TokenVerb _ synonyms)
    | lowerCaseWord `elem` synonyms = Just (TokenMatch word [token])
    | otherwise = Nothing
        where lowerCaseWord = map Data.Char.toLower word
tokenize word token@(TokenNoun _ synonyms)
    | word `elem` synonyms = Just (TokenMatch word [token])
    | lowerCaseWord `elem` synonyms = Just (TokenMatch word [token])
    | otherwise = Nothing 
        where lowerCaseWord = map Data.Char.toLower word

-- | This is a lexTokens function, along with lexInput it's responsible for matching the input with correct tokens
lexTokens :: [Token] -> [String] -> [(Maybe TokenMatch, [String])] -> [TokenMatch]
lexTokens potentialTokens wordsList [] = lexInput potentialTokens wordsList
lexTokens potentialTokens wordsList ((Nothing, _) : tokens) = lexTokens potentialTokens wordsList tokens
lexTokens potentialTokens _ ((Just token, tokenWords) : _) = token : lexInput potentialTokens tokenWords

-- | This is a lexInput function, responsible for matching the input with correct tokens
lexInput :: [Token] -> [String] -> [TokenMatch]
lexInput _ [] = []
lexInput potentialTokens (word1 : word2 : word3 : word4 : word5 : wordsList) =
    lexTokens potentialTokens (word2 : word3 : word4 : word5 : wordsList) [(foldl (\acc token -> (tokenize (word1 ++ ' ' : word2 ++ ' ' : word3 ++ ' ' : word4 ++ ' ' : word5) token) `join` acc) Nothing potentialTokens, wordsList),
                                                                       (foldl (\acc token -> (tokenize (word1 ++ ' ' : word2 ++ ' ' : word3 ++ ' ' : word4) token) `join` acc) Nothing potentialTokens, word5 : wordsList),
                                                                       (foldl (\acc token -> (tokenize (word1 ++ ' ' : word2 ++ ' ' : word3) token) `join` acc) Nothing potentialTokens, word4 : word5 : wordsList),
                                                                       (foldl (\acc token -> (tokenize (word1 ++ ' ' : word2) token) `join` acc) Nothing potentialTokens, word3 : word4 : word5 : wordsList),
                                                                       (foldl (\acc token -> (tokenize word1 token) `join` acc) Nothing potentialTokens, word2 : word3 : word4 : word5 : wordsList)]
lexInput potentialTokens (word1 : word2 : word3 : word4 : wordsList) =
    lexTokens potentialTokens (word2 : word3 : word4 : wordsList) [(foldl (\acc token -> (tokenize (word1 ++ ' ' : word2 ++ ' ' : word3 ++ ' ' : word4) token) `join` acc) Nothing potentialTokens, wordsList),
                                                               (foldl (\acc token -> (tokenize (word1 ++ ' ' : word2 ++ ' ' : word3) token) `join` acc) Nothing potentialTokens, word4 : wordsList),
                                                               (foldl (\acc token -> (tokenize (word1 ++ ' ' : word2) token) `join` acc) Nothing potentialTokens, word3 : word4 : wordsList),
                                                               (foldl (\acc token -> (tokenize word1 token) `join` acc) Nothing potentialTokens, word2 : word3 : word4 : wordsList)]
lexInput potentialTokens (word1 : word2 : word3 : wordsList) =
    lexTokens potentialTokens (word2 : word3 : wordsList) [(foldl (\acc token -> (tokenize (word1 ++ ' ' : word2 ++ ' ' : word3) token) `join` acc) Nothing potentialTokens, wordsList),
                                                       (foldl (\acc token -> (tokenize (word1 ++ ' ' : word2) token) `join` acc) Nothing potentialTokens, word3 : wordsList),
                                                       (foldl (\acc token -> (tokenize word1 token) `join` acc) Nothing potentialTokens, word2 : word3 : wordsList)]
lexInput potentialTokens (word1 : word2 : wordsList) =
    lexTokens potentialTokens (word2 : wordsList) [(foldl (\acc token -> (tokenize (word1 ++ ' ' : word2) token) `join` acc) Nothing potentialTokens, wordsList),
                                               (foldl (\acc token -> (tokenize word1 token) `join` acc) Nothing potentialTokens, word2 : wordsList)]
lexInput potentialTokens (word : wordsList) =
    lexTokens potentialTokens wordsList [(foldl (\acc token -> (tokenize word token) `join` acc) Nothing potentialTokens, wordsList)]