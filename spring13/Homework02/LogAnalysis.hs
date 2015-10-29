{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Data.Maybe
import Log

parseMessage :: String -> LogMessage

parseMessage s = case words s of
                   ("I":t:msg) -> LogMessage Info (read t) (unwords msg)
                   ("W":t:msg) -> LogMessage Warning (read t) (unwords msg)
                   ("E":sev:t:msg) -> LogMessage (Error $ read sev) (read t) (unwords msg)
                   _ -> Unknown s

parse :: String -> [LogMessage]

parse s = map parseMessage $ lines s



------



insert :: LogMessage -> MessageTree -> MessageTree

insert (Unknown _) node = node

insert m Leaf = Node Leaf m Leaf

insert m@(LogMessage _ t _) (Node l' m'@(LogMessage _ t' _) r')
  | t <= t' = Node (insert m l') m' r'
  | t > t' = Node l' m' (insert m r')

build :: [LogMessage] -> MessageTree

build lst = foldl (flip insert) Leaf lst

inOrder :: MessageTree -> [LogMessage]

inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r
inOrder Leaf = []

whatWentWrong :: [LogMessage] -> [String]

whatWentWrong = mapMaybe getMsg . filter filterExp . inOrder . build
                where
                  filterExp (LogMessage (Error sev) _ _) = sev >= 50
                  filterExp _ = False
                  getMsg (LogMessage _ _ s) = Just s
                  getMsg _ = Nothing
