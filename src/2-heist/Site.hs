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
-- import           Data.Monoid
import qualified Data.Text as T
import           Text.Read as R
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
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
handleFactorial1 :: Handler App App ()
handleFactorial1 = do
    mbNumber <- getParam "number"
    case readNumber mbNumber of
      Just n -> renderWithSplices "Factorial1" $ do
            "number" ## nr n
            "factorial" ## (nr $ factorial n)
      _ -> writeBS "must specify factorial/number in URL"
  where nr n = return $ renderHtmlNodes $ H.toMarkup n

factorial2Form :: Monad m => Integer -> Form T.Text m Integer
factorial2Form n = 
    (\n' -> read $ T.unpack n')
    <$> "number" .: check "Must be non-empty && a number > 0" ncheck
                          (D.text $ Just $ T.pack $ show n)
  where
    ncheck n = (not $ T.null n) && isDigit (T.head n) && isJust mn && n' > 0
      where mn@(~(Just (n' :: Integer))) = R.readMaybe $ T.unpack n

handleFactorial2 :: String -> Integer -> Handler App App ()
handleFactorial2 postAction n = do
    (formView, formResult) <- DS.runForm "form" $ factorial2Form n
    case formResult of
      Just n' -> redirect $ B.pack $ "/factorial1/" ++ show n'
      _ -> do
          heistLocal (bindDigestiveSplices formView)
            $ renderWithSplices "Factorial2" $ do
                "postAction" ## (I.textSplice $ T.pack postAction)
  -- where nr n = I.textSplice $ T.pack $ show n

-- | This version does not really work because state is not propagated,
-- included here as a stepping stone to the use of acid in later versions
handleFactorial3 :: String -> Integer -> Handler App App ()
handleFactorial3 postAction n = do
    (formView, formResult) <- DS.runForm "form" $ factorial2Form n
    case formResult of
      Just _ -> redirect $ B.pack postAction
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
         , ("/factorial/:number" , handleFactorial1)
         , ("/factorial1/:number", handleFactorial1)
         , ("/factorial2"        , handleFactorial2 "factorial2" 5)
         , ("/factorial3"        , handleFactorial3 "factorial3" 5)
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
    addRoutes routes
    addAuthSplices h auth
    return $ App h s a

