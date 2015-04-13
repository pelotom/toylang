module Toy.Expression where

import Data.Tuple
import Data.Maybe
import Data.Foldable

type Idx = Number

data Expr = Var Idx
          | Abs Expr
          | App Expr Expr

instance eqExpr :: Eq Expr where
  (==) (Var x) (Var y) = x == y
  (==) (Abs e1) (Abs e2) = e1 == e2
  (==) (App a1 b1) (App a2 b2) = a1 == a2 && b1 == b2
  (==) _ _ = false
  (/=) e1 e2 = not $ e1 == e2

instance showExpr :: Show Expr where
  show (Var x) = show x
  show (Abs e) = "# " ++ show e
  show (App e1 e2) = paren true e1 ++ " " ++ paren true e2
    where
      paren _ (Var x) = show x
      paren true e@(App _ _) = show e
      paren _ e = "(" ++ show e ++ ")"

-- Substitute e1 for index i in e2
subst :: Expr -> Idx -> Expr -> Expr
subst e = go
  where
    go i v@(Var j) = if i == j then e else v
    go i (App e1 e2) = App (go i e1) (go i e2)
    go i (Abs e') = Abs (go (i + 1) e')

-- Evaluate an expression by a single step, if possible
stepEval :: Expr -> Maybe Expr
stepEval (App (Abs a) b) = Just $ subst b 0 a
stepEval (App a b) = flip App b <$> stepEval a
stepEval _ = Nothing


