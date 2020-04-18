{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-|
  Description: Contains functions relating to home-manager’s rebuild functionality
Contains functions relating to home-manager’s rebuild functionality
  -}
module NixManager.HMRebuild
  ( rebuild
  )
where

import           NixManager.PosixTools          ( mkdir
                                                , cp
                                                )
import           NixManager.Bash                ( Expr(Command)
                                                , (&&.)
                                                )
import           NixManager.HMRebuildMode       ( HMRebuildMode
                                                  ( RebuildSwitch
                                                  , RebuildDrySwitch
                                                  )
                                                )
import           NixManager.Process             ( runProcess
                                                , noStdin
                                                , ProcessData
                                                )
import           NixManager.HMPackagesUtil      ( locatePendingPackagesFileMaybeCreate
                                                )
import           NixManager.HMServicesUtil      ( locatePendingServicesFileMaybeCreate
                                                )
import           System.Directory               ( getXdgDirectory
                                                , XdgDirectory(XdgCache)
                                                )
import           NixManager.Constants           ( appName )

rebuildExpr :: HMRebuildMode -> Expr
rebuildExpr RebuildSwitch    = Command "home-manager" ["switch"]
rebuildExpr RebuildDrySwitch = Command "home-manager" ["-n", "switch"]

rebuildAndSaveExpr :: HMRebuildMode -> IO Expr
rebuildAndSaveExpr RebuildSwitch = do
  cacheDir        <- getXdgDirectory XdgCache appName
  pendingPackages <- locatePendingPackagesFileMaybeCreate
  pendingServices <- locatePendingServicesFileMaybeCreate
  pure
    (   mkdir True [cacheDir]
    &&. rebuildExpr RebuildSwitch
    &&. cp pendingPackages cacheDir
    &&. cp pendingServices cacheDir
    )
rebuildAndSaveExpr RebuildDrySwitch = pure (rebuildExpr RebuildDrySwitch)

rebuild :: HMRebuildMode -> IO ProcessData
rebuild mode = rebuildAndSaveExpr mode >>= runProcess noStdin
