{-# OPTIONS_GHC -Wall #-}

import Log

isInteger :: String -> Bool
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

isDouble :: String -> Bool
isDouble s = case reads s :: [(Double, String)] of
  [(_, "")] -> True
  _         -> False

isNumeric :: String -> Bool
isNumeric s = isInteger s || isDouble s

parseMessage :: String -> LogMessage
parseMessage msg = par $ (splitAt 2 . words) msg where 
  par (["E", time], xs) | isNumeric $ head xs = LogMessage (Error (read time::Int)) (read (head xs)::Int) (unwords $ tail xs) 
  par (["W", time], xs) | isNumeric time      = LogMessage Warning (read time::Int) (unwords xs)
  par (["I", time], xs) | isNumeric time      = LogMessage Info (read time::Int) (unwords xs)
  par _ = Unknown msg

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = newNode msg
insert msg (Node Leaf node rightChild)
  | msg < node = Node (newNode msg) node rightChild
insert msg (Node leftChild node Leaf)
  | msg > node = Node leftChild node (newNode msg)
insert msg (Node leftChild node rightChild)
  | msg < node = Node (insert msg leftChild) node rightChild
  | otherwise = Node leftChild node (insert msg rightChild)

newNode :: LogMessage -> MessageTree
newNode m = Node Leaf m Leaf

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l msg r) = inOrder l ++ [msg] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msg = map extractMessage $ filter (severe 50) msg

extractMessage :: LogMessage -> String
extractMessage (LogMessage _ ts msg) = show ts ++ " " ++ msg
extractMessage _ = error "Malformed log entry." 

severe:: Int -> LogMessage -> Bool
severe minLvl (LogMessage (Error lvl) _ _)
  | lvl > minLvl  = True
  | otherwise     = False
severe _ _ = False

