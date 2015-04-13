module Main where

import Toy.Expression
import Data.Maybe
import Control.Monad
import Control.Monad.Eff
import Debug.Trace

v0 :: Expr
v0 = Var 0

v1 :: Expr
v1 = Var 1

v2 :: Expr
v2 = Var 2

testExpr :: Expr
testExpr = App (Abs $ App v0 v2) (App (Abs $ App v0 v0) (Abs $ v1))

diverge :: Expr
diverge = App (Abs $ App v0 v0) (Abs $ App v0 v0)

idExpr :: Expr
idExpr = Abs $ Var 0

y :: Expr
y = Abs $ App foo foo
  where
    foo = Abs $ App v1 (App v0 v0)

diverge2 :: Expr
diverge2 = App y idExpr

traceEval :: forall e. Expr -> Eff (trace :: Trace | e) Unit
traceEval e = do
  trace "Starting evaluation:"
  trace $ "0     " ++ show e
  go 1 e
  trace "Finished evaluating."
    where
      go :: forall e. Number -> Expr -> Eff (trace :: Trace | e) Unit
      go n e = do
        maybe (return unit) (\next -> do
            trace $ show n ++ " ==> " ++ show next
            go (n+1) next
          ) (stepEval e)

main = traceEval testExpr
