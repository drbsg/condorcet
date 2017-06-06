{-# LANGUAGE FlexibleContexts #-}
-- Condorcet voting.
-- Copyright (C) 2005 Evan Martin <martine@danga.com>

module Main where

import qualified Condorcet

import Text.ParserCombinators.Parsec
import Control.Monad.Except

{- | Parses a simple form of ballots as strings.
   Ballot strings are comma-separated list in order of preference;
   equally-ranked candidates are separated with an equals sign.
   E.g., "3=4=5,1,2" -}
parseBallot :: String -> Either ParseError Condorcet.Ballot
parseBallot input = parse ballot "" input where
  ballot :: Parser [[Int]]
  ballot = do { b <- sepBy1 rank (char ','); eof; return b }
  rank :: Parser [Int]
  rank = sepBy1 number (char '=')
  number :: Parser Int
  number = do { ds <- many1 digit; return (read ds) }

-- parse a set of ballots, halting on failure (using the Error monad).
parseBallots :: [String] -> Either String [Condorcet.Ballot]
parseBallots bs = mapM pb bs >>= return where
  pb ballot = case parseBallot ballot of
                Left err -> throwError (show err)
                Right x  -> return x

main :: IO ()
main = do
  case parseBallots ["1,2,3", "2=1,3", "2,1,3"] of
    Left e -> putStrLn e
    Right ballots -> do
      putStrLn $ "Parsed " ++ (show (length ballots)) ++ " ballots:"
      mapM_ print ballots
      putStrLn "Computing winners..."
      let winners = Condorcet.run ballots
      putStrLn "Winners:"
      putStrLn $ show winners

-- vim: set ts=2 sw=2 et :

