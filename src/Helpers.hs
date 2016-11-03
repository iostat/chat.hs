module Helpers where

import Data.Map.Strict as Map hiding (foldl)

import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit

-- Takes a list of OptDescrs which will transform a structure that holds the actual opt
-- a default set of opts
-- and a function that takes a helper for handling reporting errors and exiting and parsed opts
-- and runs it
mainHelper :: [OptDescr (a -> IO a)]
           -> a
           -> ((String -> IO b) -> a -> IO b)
           -> IO b
mainHelper optDescrs defaultOpts prettyMain =
    let haltAndCatchFire = (\msg -> putStrLn msg >> exitFailure) in do
        args <- getArgs
        let (matched,_,_) = getOpt Permute optDescrs args
        matchedOpts <- foldl (>>=) (return defaultOpts) matched
        prettyMain haltAndCatchFire matchedOpts

unwordsToMaybe :: [String] -> Maybe String
unwordsToMaybe [] = Nothing
unwordsToMaybe xs = Just . unwords $ xs
