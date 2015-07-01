{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString.Char8 as B
import           Data.Char
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Text.Read as R
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Snap.Snaplet.AcidState
import           Heist
import qualified Heist.Interpreted as I
import           Text.Digestive as D
import qualified Text.Digestive.Snap as DS
import           Text.Digestive.Heist
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Renderer.XmlHtml
------------------------------------------------------------------------------
import           Application
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | App logic: factorial
factorial :: Integer -> Integer
factorial n = product [1..n]

readNumber :: Maybe ByteString -> Maybe Integer
readNumber mn = fmap B.unpack mn >>= R.readMaybe

------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe mempty splice authError
    splice err = "loginError" ## I.textSplice err


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"


------------------------------------------------------------------------------
-- | Handle factorial
factorial3Form :: Monad m => Integer -> Form T.Text m Integer
factorial3Form n = 
    (\n' -> read $ T.unpack n')
    <$> "number" .: check "Must be non-empty && a number > 0" ncheck
                          (D.text $ Just $ T.pack $ show n)
  where
    ncheck n = (not $ T.null n) && isDigit (T.head n) && isJust mn && n' > 0
      where mn@(~(Just (n' :: Integer))) = R.readMaybe $ T.unpack n

handleFactorial3 :: String -> Handler App App ()
handleFactorial3 postAction = do
    n <- query AcidNr
    (formView, formResult) <- DS.runForm "form" $ factorial3Form n
    case formResult of
      Just n' -> do
          update $ AcidSetNr n'
          redirect $ B.pack postAction
      _ -> do
          heistLocal (bindDigestiveSplices formView)
            $ renderWithSplices "Factorial3" $ do
                "postAction" ## (I.textSplice $ T.pack postAction)
                "factorial" ## (nr $ factorial n) 
  where nr n = I.textSplice $ T.pack $ show n

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",    with auth handleLoginSubmit)
         , ("/logout",   with auth handleLogout)
         , ("/factorial3"        , handleFactorial3 "factorial3")
         , ("/new_user", with auth handleNewUser)
         , ("",          serveDirectory "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"
    c  <- nestSnaplet "acid" acid $ acidInit $ AppAcid 5
    addRoutes routes
    addAuthSplices h auth
    return $ App h s a c

