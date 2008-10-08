#!/usr/bin/runhaskell

import qualified Control.Monad
import qualified Data.List as List
import qualified System.IO
import qualified Text.Regex

data State =
    RestState
  | ProveState
  deriving (Eq, Show, Ord)

getLines :: System.IO.Handle -> IO [String]
getLines h =
    do s <- System.IO.hGetContents h
       return (lines s)

excludeTheorems :: [String]
excludeTheorems =
    ["COND_EQ_CLAUSE"]

proveRegex :: Text.Regex.Regex
proveRegex = Text.Regex.mkRegex "^let +([[:alnum:]_]+) *= *prove *$"

endRegex :: Text.Regex.Regex
endRegex = Text.Regex.mkRegex "(;;) *$"

process :: State -> String -> (State,String)
process s l =
    case s of
      RestState ->
          case Text.Regex.matchRegexAll proveRegex l of
            Just (_,_,_,[l1]) ->
                if l1 `elem` excludeTheorems
                  then (s,l)
                  else (ProveState, "let " ++ l1 ++ " = lemma (fun () -> prove")
            Just _ -> error "bad match"
            Nothing -> (s,l)
      ProveState ->
          case Text.Regex.matchRegexAll endRegex l of
            Just (l1,_,_,[l2]) ->
                (RestState, l1 ++ ")" ++ l2)
            Just _ -> error "bad match"
            Nothing -> (s,l)

main :: IO ()
main =
    do ls <- getLines System.IO.stdin
       let (s,ls') = List.mapAccumL process RestState ls
       case s of
         RestState -> return ()
         ProveState -> error "eof inside a prove"
       Control.Monad.forM_ ls' putStrLn
