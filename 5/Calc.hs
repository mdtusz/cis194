{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import StackVM as VM

-- Q1
eval :: ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Mul x y) = eval x * eval y 

-- Q2
evalStr :: String -> Maybe Integer
evalStr str =
  case expr of
    Just expr -> Just $ eval expr
    otherwise -> Nothing
  where  expr = parseExp ExprT.Lit ExprT.Add ExprT.Mul str

-- Q3
class Expr a where 
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where 
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id

-- Q4
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (0<)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

instance Expr Mod7 where
  lit x = Mod7 $ x `mod` 7
  add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod`  7
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7

-- Q5
instance Expr VM.Program where
  lit x = [VM.PushI x]
  add x y = x ++ y ++ [VM.Add]
  mul x y = x ++ y ++ [VM.Mul]

compile :: String -> Maybe VM.Program
compile str = parseExp lit add mul str :: Maybe Program

class HasVars a where
  var :: String -> a

  
