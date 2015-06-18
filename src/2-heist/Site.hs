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
import           Heist
import qualified Heist.Interpreted as I
import           Text.Digestive as D
import qualified Text.Digestive.Snap as DS
import           Text.Digestive.Heist
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
    maybe (writeBS "must specify factorial/number in URL")
          (\n -> renderWithSplices "Factorial1" $ do
            "number" ## nr n -- (I.textSplice $ T.pack postAction)
            "factorial" ## (nr $ factorial n) -- (I.textSplice $ T.pack postAction)
          )
          (readNumber mbNumber)
  where nr n = I.textSplice $ T.pack $ show n

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
                "factorial" ## (nr $ factorial n) 
  where nr n = I.textSplice $ T.pack $ show n


{-
handleEditUser :: String -> Handler App App ()
handleEditUser postAction = do
    mbuser <- getCurrentUser
    case mbuser of
      Just user -> do
        let uid = userKey user
        (formView, formResult) <- DS.runForm "form" $ editFormUser user
        case formResult of
          Just user2 -> do
            with acid $ update $ UserSetByKeyAcid uid user2
            defaultHandler
          _ -> do
            blogAndUsers <- query $ BlogsSelectByMbUserAcid (Just uid)
            heistLocal (bindDigestiveSplices formView)
              $ renderWithSplices "userEditForm" $ do
                  "postAction" ## (I.textSplice $ T.pack postAction)
                  "blogs" ##
                    (return $ renderHtmlNodes $ flip hrenderBlogs blogAndUsers $ \blog ->
                       H.a H.! HA.href (fromString $ handlernameMkTop $ mkEditBlogHdlrNm handlernameEditBlog (blog ^. blogId))
                         $ "edit"
                    )
      _ -> defaultHandler

editFormUser :: Monad m => User -> Form T.Text m User
editFormUser u =
    (\n pw e ->
      (userName .~ n) .
      (authUser %~ \au -> au
           { userEmail = Just e
           , userPassword = if T.null pw then userPassword au else (Just $ encr $ ClearText $ B.pack $ T.unpack pw)
           }) $
        u
    )
    <$> "name"      .: checkNonEmpty (D.text (Just $ u ^. userName))
    <*> "password"  .: D.text Nothing
    <*> "email"     .: check "Not a valid email address" checkEmail (D.text (userEmail $ u ^. authUser))
  where
    checkEmail e | T.null e  = True
                 | otherwise = isJust $ T.find (== '@') e
    encr pw = unsafePerformIO $ encryptPassword pw

-}

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",    with auth handleLoginSubmit)
         , ("/logout",   with auth handleLogout)
         , ("/factorial/:number" , handleFactorial1)
         , ("/factorial1/:number", handleFactorial1)
         , ("/factorial2"        , handleFactorial2 "factorial2" 5)
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

