{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Foundation where
import Import
import Yesod
import Data.Text
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

data Sitio = Sitio { connPool :: ConnectionPool }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Post
   titulo Text
   conteudo Text
   categoria Text
   autor Text
   deriving Show

User
    login Text
    senha Text
    deriving Show

Contato
    assunto Text
    mensagem Text
    deriving Show
|]

mkYesodData "Sitio" pRoutes

instance YesodPersist Sitio where
   type YesodPersistBackend Sitio = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool


instance Yesod Sitio where
    authRoute _ = Just $ LoginR
    isAuthorized HomeR _ = return Authorized
    isAuthorized QuemSomosR _ = return Authorized
    isAuthorized MakeR _ = return Authorized
    isAuthorized ModaR _ = return Authorized
    isAuthorized ContatoR _ = return Authorized
    isAuthorized CadastroPostR _ = isUser
    isAuthorized CadastroUserR _ = isUser
    isAuthorized ListarMsgsR _ = isUser
    isAuthorized LoginR _ = return Authorized
    isAuthorized _ _ = isUser

isUser = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Sitio FormMessage where
    renderMessage _ _ = defaultFormMessage
