{-# LANGUAGE OverloadedStrings #-}
{-|
  Description: Contains functions related to collecting garbage as a user (e.g. “with home-manager”)
 -}
module NixManager.HMGarbage
  ( collectGarbage
  )
where

import           NixManager.Bash                ( Expr(Command) )
import           NixManager.Process             ( runProcess
                                                , noStdin
                                                , ProcessData
                                                )

collectGarbageExpr :: Expr
collectGarbageExpr = Command "nix-collect-garbage" mempty

collectGarbage :: IO ProcessData
collectGarbage = runProcess noStdin collectGarbageExpr
