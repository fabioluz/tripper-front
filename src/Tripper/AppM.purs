module Tripper.AppM where

import Tripper.Prelude
import Tripper.Config

import Tripper.Feature.Navigation.Capability (class Navigate)
import Tripper.Feature.Auth.Capability (class ManageAuth)
import Tripper.Feature.Translation.Capability (class I18N)
import Tripper.Feature.User.Capability (class ManageUser)
import Type.Equality (class TypeEquals, from)

import Tripper.Feature.Navigation.Effects as NavigateAff
import Tripper.Feature.Auth.Effects as AuthAff
import Tripper.Feature.Translation.Effects as I18NAff
import Tripper.Feature.User.Effects as UserAff

newtype AppM a = AppM (ReaderT Config Aff a)

runAppM :: Config -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM     :: Functor AppM
derive newtype instance applyAppM       :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM        :: Bind AppM
derive newtype instance monadAppM       :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM    :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Config => MonadAsk e AppM where
  ask = AppM $ asks from

instance navigateAppM :: Navigate AppM where
  navigate = NavigateAff.navigate

instance manageAuthAppM :: ManageAuth AppM where
  getUserBus     = AuthAff.getUserBus
  identifyUser   = AuthAff.identifyUser
  loginUser      = AuthAff.loginUser
  logoutUser     = AuthAff.logoutUser
  registerClient = AuthAff.registerClient

instance manageUserAppM :: ManageUser AppM where
  getMe    = UserAff.getMe
  getUsers = UserAff.getUsers

instance i18nAppM :: I18N AppM where
  setLanguage = I18NAff.setLanguage
  translate   = I18NAff.translate
