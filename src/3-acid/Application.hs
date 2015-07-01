{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeFamilies, DeriveDataTypeable #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.Typeable
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Session
import           Snap.Snaplet.AcidState
import           Data.SafeCopy (base, deriveSafeCopy)

------------------------------------------------------------------------------
data AppAcid = AppAcid
    { _nr   :: Integer
    }
    deriving (Typeable)

makeLenses ''AppAcid

deriveSafeCopy 0 'base ''AppAcid

data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    , _auth :: Snaplet (AuthManager App)
    , _acid :: Snaplet (Acid AppAcid)
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasAcid App AppAcid where
  getAcidStore  = view $ acid . snapletValue

------------------------------------------------------------------------------
type AppHandler = Handler App App

------------------------------------------------------------------------------
-- The Acid API

-- | Get the number for factorial
acidNr :: Query AppAcid Integer
acidNr = view nr

-- | Get the number for factorial
acidSetNr :: Integer -> Update AppAcid ()
acidSetNr n = nr .= n


------------------------------------------------------------------------------
-- Acidification
makeAcidic ''AppAcid
  [ 'acidNr
  , 'acidSetNr
  ]
