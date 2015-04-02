module Main where

import Toy.Expression
import Data.Maybe
import Control.Monad
import Control.Monad.Eff
import Debug.Trace

term1 :: Expr
term1 = App (Abs "f" (App (Var "f") (Var "z"))) (App (Abs "g" (App (Var "g") (Var "g"))) (Abs "x" $ Var "x"))

diverge :: Expr
diverge = App xToXX xToXX
  where
    xToXX = Abs "x" (App (Var "x") (Var "x"))

traceEval :: forall e. Expr -> Eff (trace :: Trace | e) Unit
traceEval e = do
  trace "Starting evaluation:"
  trace $ "0    " ++ show e
  go 1 e
  trace "Finished evaluating."
    where
      go :: forall e. Number -> Expr -> Eff (trace :: Trace | e) Unit
      go n e = do
        maybe (return unit) (\next -> do
            trace $ show n ++ " => " ++ show next
            go (n+1) next
          ) (stepEval e)

main = traceEval term1
