module Tripper.Feature.Auth.Capability where
  
import Tripper.Prelude
import Effect.Ref (Ref)
import Effect.Aff.Bus (BusRW)
import Halogen (HalogenM)

type CurrentUser =
  { clientId :: String
  , userId   :: String
  , name     :: String
  }

type LoginFields =
  { email    :: String
  , password :: String
  }

type LoginRes =
  { token :: String
  , user  :: CurrentUser
  }

type RegisterFields = 
  { clientName     :: String
  , adminEmail     :: String
  , adminPassword  :: String
  , adminCPassword :: String
  , adminName      :: String
  , adminNickName  :: Maybe String
  }

class HasCurrentUser env where
  curUserRef :: env -> Ref (Maybe CurrentUser)
  curUserBus :: env -> BusRW (Maybe CurrentUser)

class MonadAff m <= ManageAuth m where
  getUserBus     :: m (BusRW (Maybe CurrentUser))
  identifyUser   :: m Unit
  loginUser      :: LoginFields -> m (Either Error LoginRes)
  logoutUser     :: m Unit
  registerClient :: RegisterFields -> m (Either Error Unit)

instance manageAuthHalogenM :: ManageAuth m => ManageAuth (HalogenM st act sl out m) where
  getUserBus     = lift getUserBus
  identifyUser   = lift identifyUser
  logoutUser     = lift logoutUser
  loginUser      = lift <<< loginUser
  registerClient = lift <<< registerClient