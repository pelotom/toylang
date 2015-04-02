module Main where

import Toy.Expression
import Control.Monad
import Control.Monad.Eff
import Debug.Trace

term1 :: Expr
term1 = App (Abs "f" (App (App (Var "f") (Var "x")) (Var "y"))) (Abs "g" (App (Var "g") (Var "y")))

diverge :: Expr
diverge = App xToXX xToXX
  where
    xToXX = Abs "x" (App (Var "x") (Var "x"))

traceEval :: forall e. Expr -> Eff (trace :: Trace | e) Unit
traceEval e = do
  trace "Starting evaluation:"
  trace $ "0    " ++ show e
  go 1 e
  trace "Done evaluating!"
    where
      go :: forall e. Number -> Expr -> Eff (trace :: Trace | e) Unit
      go n e = do
        when (not $ isValue e) $ do
          let next = stepEval e
          trace $ show n ++ " => " ++ show next
          go (n+1) next

main = traceEval term1
