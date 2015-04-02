module Toy.Expression where

import Data.Tuple
import Data.Maybe
import Data.Foldable

type Id = String

data Expr = Var Id
          | Abs Id Expr
          | App Expr Expr

instance eqExpr :: Eq Expr where
  (==) (Var x) (Var y) = x == y
  (==) (Abs x1 e1) (Abs x2 e2) = x1 == x2 && e1 == e2
  (==) (App a1 b1) (App a2 b2) = a1 == a2 && b1 == b2
  (/=) e1 e2 = not $ e1 == e2

instance showExpr :: Show Expr where
  show = pretty

pretty (Var x) = x
pretty e@(Abs _ _) = case stripBoundVars e of
    Tuple xs e' -> "\\" ++ intercalate " " xs ++ " -> " ++ pretty e'
  where
    stripBoundVars :: Expr -> Tuple [Id] Expr
    stripBoundVars (Abs x e) = case stripBoundVars e of
      Tuple xs e' -> Tuple (x : xs) e'
    stripBoundVars e = Tuple [] e
    
pretty (App e1 e2) = paren true e1 ++ " " ++ paren false e2
  where
    paren _ (Var x) = x
    paren true e@(App _ _) = pretty e
    paren _ e = "(" ++ pretty e ++ ")"

stepEval :: Expr -> Maybe Expr
stepEval (App (Abs x e) e') = Just $ replace x e' e
  where
    replace :: Id -> Expr -> Expr -> Expr
    replace x e = go
      where
        go v@(Var y) = if x == y then e else v
        go (App e' e'') = App (go e') (go e'')
        go (Abs y e') = Abs y (go e') -- FIXME: avoid capture
stepEval (App e e') = (\e'' -> App e'' e') <$> stepEval e
stepEval _ = Nothing


