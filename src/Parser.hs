{-# OPTIONS -Wall #-}
{- | Copyright Laurence Emms 2018.
Module responsible for creating a Sentence. Simplified compared to original version by Emms.
-}
module Parser where

import Lexer

{- | Example of Sentence type.

__Examples:__

@
pharse = Pharse (TokenVerb "run" ["run","dash","race"])
simpleSentence = SimpleSentence ((TokenVerb "look at" ["look at","analyze","study"]) (TokenNoun "gem" ["gem","jewel","shiny stone"])
@
-}
data Sentence = NullSentence -- ^ Null constructor for completion
                | Phrase Token -- ^ First constructor 
                | SimpleSentence Token Token -- ^ Second constructor
                    deriving (Show, Eq)

-- | Checking if verbs, given as tokens, are in the given list of tokens
verbsInTokenList :: [Token] -> [Token]
verbsInTokenList [] = []
verbsInTokenList (verb@(TokenVerb _ _) : ts) = verb : verbsInTokenList ts
verbsInTokenList (_ : ts) = verbsInTokenList ts

-- | Checking if nouns, given as tokens, are in the given list of tokens
nounsInTokenList :: [Token] -> [Token]
nounsInTokenList [] = []
nounsInTokenList (noun@(TokenNoun _ _) : ts) = noun : nounsInTokenList ts
nounsInTokenList (_ : ts) = nounsInTokenList ts

-- | This function makes all possible sentences from two given lists of tokens (assumed to be verbs and then nouns)
makeSentence :: [[Token]] -> [Sentence]
makeSentence []
    = []
makeSentence [verbs]
    = fmap (\verb -> Phrase verb) verbs
makeSentence [verbs, nouns]
    = fmap (\[verb, noun] -> SimpleSentence verb noun) 
      ((:) <$> verbs <*>
           ((:) <$> nouns <*> [[]]))
makeSentence _ = []

-- | This function creates sentences from token matches
parseSentence :: [TokenMatch] -> [Sentence]
parseSentence [(TokenMatch _ t0), (TokenMatch _ t1)]
    = makeSentence [(verbsInTokenList t0),
                    (nounsInTokenList t1)]
parseSentence [(TokenMatch _ t0)]
    = makeSentence [(verbsInTokenList t0)]
parseSentence _ = []