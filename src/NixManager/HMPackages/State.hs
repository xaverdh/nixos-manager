{-|
  Description: Contains all the state for the Packages tab
  -}
module NixManager.HMPackages.State
  ( State
  , initState
  )
where

import           NixManager.HMPackagesUtil      ( readPackageCache )
import           NixManager.Util                ( TextualError
                                                , ifSuccessIO
                                                )
import qualified NixManager.View.PackageEditView
                                               as PackageEditView

type State = PackageEditView.State

-- | The initial Packages tab state (needs to read the package cache, hence the side-effect)
initState :: IO (TextualError State)
initState =
  ifSuccessIO readPackageCache (pure . Right . PackageEditView.initState)

