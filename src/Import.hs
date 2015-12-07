{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod

pRoutes = [parseRoutes|
    / HomeR GET
    /quemsomos QuemSomosR GET
    /make MakeR GET
    /moda ModaR GET
    /postar CadastroPostR GET POST
    /cadastrar CadastroUserR GET POST
    /listarUser ListarUserR GET
    /contato ContatoR GET POST
    /listarMsgs ListarMsgsR GET
    /login LoginR GET POST
    /home HomeUserR GET
    /bye ByeR GET
|]