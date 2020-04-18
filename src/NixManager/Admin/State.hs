{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
  Description: Contains all the state for the Administration tab
  -}
module NixManager.Admin.State
  ( State(..)
  , asChanges
  , asRebuildData
  , asGarbageData
  , initState
  , determineChanges
  )
where

import           NixManager.ChangeType          ( ChangeType(Changes, NoChanges)
                                                )
import           Control.Lens                   ( makeLenses )
import           NixManager.Admin.RebuildData   ( RebuildData
                                                , initialRebuildData
                                                )
import           NixManager.Admin.GarbageData   ( GarbageData
                                                , initialGarbageData
                                                )
import           NixManager.NixServicesUtil     ( locateLocalServicesFile
                                                , locateRootServicesFile
                                                )
import           NixManager.NixPackagesUtil     ( locateLocalPackagesFile
                                                , locateRootPackagesFile
                                                )
import           NixManager.Util                ( determineFilesEqual )

-- | Contains all the state for the administration tab
data State = State {
    _asRebuildData :: RebuildData -- ^ The “Rebuild” GUI state
  , _asGarbageData :: GarbageData -- ^ The “Collect garbage” GUI state
  , _asChanges :: ChangeType -- ^ Information about whether we have unapplied changes
  }

makeLenses ''State


-- | Determine if there are changes that have to be applied.
determineChanges :: IO ChangeType
determineChanges = do
  packagesEqual <- determineFilesEqual locateLocalPackagesFile
                                       locateRootPackagesFile
  servicesEqual <- determineFilesEqual locateLocalServicesFile
                                       locateRootServicesFile
  pure (if packagesEqual && servicesEqual then NoChanges else Changes)


-- | The initial Administation tab state (needs to determine changes, hence the side-effect)
initState :: IO State
initState = State initialRebuildData initialGarbageData <$> determineChanges
