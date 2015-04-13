module Toy.Expression where

import Data.Tuple
import Data.Maybe
import Data.Foldable

type Idx = Number

data Expr = Var Idx
          | Abs Expr
          | App Expr Expr

instance showExpr :: Show Expr where
  show (Var x) = show x
  show (Abs e) = "# " ++ show e
  show (App e1 e2) = paren true e1 ++ " " ++ paren true e2
    where
      paren _ (Var x) = show x
      paren true e@(App _ _) = show e
      paren _ e = "(" ++ show e ++ ")"

-- Evaluate an expression by a single step, if possible
stepEval :: Expr -> Maybe Expr
stepEval (App (Abs a) b) = Just $ subst 0 a
  where
    -- Substitute b for the given index in an expression
    subst :: Idx -> Expr -> Expr
    subst i v@(Var j) = if i == j then b else v
    subst i (App e1 e2) = App (subst i e1) (subst i e2)
    subst i (Abs e') = Abs (subst (i + 1) e')
stepEval (App a b) = flip App b <$> stepEval a
stepEval _ = Nothing

