{-# LANGUAGE OverloadedStrings #-}
module NixManager.HMRebuildMode
  ( HMRebuildMode(..)
  , rebuildModeToText
  , rebuildModeToDescription
  , rebuildModes
  , rebuildModeIdx
  )
where

import           Data.Text                      ( Text )
import           Control.Lens                   ( iso
                                                , Iso'
                                                )
import           Data.List                      ( elemIndex )
import           Data.Maybe                     ( fromJust )

data HMRebuildMode = RebuildSwitch
                   | RebuildDrySwitch
                   deriving(Eq, Ord, Enum, Bounded, Show)

rebuildModeToText :: HMRebuildMode -> Text
rebuildModeToText RebuildSwitch    = "switch"
rebuildModeToText RebuildDrySwitch = "dry-switch"

rebuildModeToDescription :: HMRebuildMode -> Text
rebuildModeToDescription RebuildSwitch = "Build and activate configuration"
rebuildModeToDescription RebuildDrySwitch =
  "Do a dry run, only print what actions would be taken"

-- | List of all rebuild modes
rebuildModes :: [HMRebuildMode]
rebuildModes = [minBound .. maxBound]

-- | The index of a rebuild mode inside the list of all rebuild modes and vice-versa.
rebuildModeIdx :: Iso' HMRebuildMode Int
rebuildModeIdx = iso (fromJust . (`elemIndex` rebuildModes)) (rebuildModes !!)
