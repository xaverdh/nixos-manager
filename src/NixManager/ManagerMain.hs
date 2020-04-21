{-|
Description: NixOS manager's /real/ entry point

This file should initialize the application state, as well as GTK, and then run gi-gtk-declarative-app-simple's main method.
 -}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.ManagerMain
  ( nixMain
  )
where

import           NixManager.ProgramArguments    ( parseArguments )
import qualified NixManager.Update             as GlobalUpdate
import qualified NixManager.Admin.State        as AdminState
import qualified NixManager.Services.State     as ServicesState
import qualified NixManager.Packages.State     as PackagesState
import qualified NixManager.HMPackages.State   as HMPackagesState
import qualified NixManager.HMServices.State   as HMServicesState
import qualified NixManager.HMAdmin.State      as HMAdminState
import           NixManager.View.ErrorDialog    ( runErrorDialog )
import           NixManager.View.Css            ( initCss )
import           NixManager.ManagerState        ( ManagerState(..) )
import           NixManager.Util                ( TextualError
                                                , ifSuccessIO
                                                )
import           NixManager.View.Root           ( view' )
import           GI.Gtk.Declarative.App.Simple  ( App(App)
                                                , view
                                                , update
                                                , inputs
                                                , initialState
                                                , run
                                                )
import           Control.Monad                  ( void )
import qualified GI.Gtk                        as Gtk
import           Prelude                 hiding ( length
                                                , putStrLn
                                                )

-- |Initialize the application state, optionally returning an error.
initState :: IO (TextualError ManagerState)
initState = ifSuccessIO PackagesState.initState $ \packagesState ->
  ifSuccessIO HMPackagesState.initState $ \hmPackagesState -> do
    serviceState   <- ServicesState.initState
    adminState     <- AdminState.initState
    hmServiceState <- HMServicesState.initState
    hmAdminState   <- HMAdminState.initState
    pure $ Right $ ManagerState { _msPackagesState   = packagesState
                                , _msServiceState    = serviceState
                                , _msAdminState      = adminState
                                , _msHMServiceState  = hmServiceState
                                , _msHMAdminState    = hmAdminState
                                , _msHMPackagesState = hmPackagesState
                                }

-- |Initialize GTK, the application state (see "NixManager.ManagerState") and run the GTK main loop. See also: "NixManager.Update" and "NixManager.View.Root"
nixMain :: IO ()
nixMain = do
  void (Gtk.init Nothing)
  initCss
  initialState' <- initState
  args          <- parseArguments
  case initialState' of
    Left  e -> runErrorDialog e
    Right s -> void $ run App { view         = view' args
                              , update       = GlobalUpdate.update
                              , inputs       = []
                              , initialState = s
                              }
