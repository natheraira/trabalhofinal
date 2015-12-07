{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers where
import Import
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Text.Lucius

import Database.Persist.Postgresql

mkYesodDispatch "Sitio" pRoutes

formPost :: Form Post
formPost = renderDivs $ Post <$>
             areq textField "Titulo" Nothing <*>
             areq textField "Conteudo" Nothing <*>
             areq textField "Categoria" Nothing <*>
             areq textField "Autor" Nothing

formUser :: Form User
formUser = renderDivs $ User <$>
             areq textField "Login" Nothing <*>
             areq passwordField "Senha" Nothing

formContato :: Form Contato
formContato = renderDivs $ Contato <$>
             areq textField "Assunto" Nothing <*>
             areq textField "Mensagem" Nothing

widgetForm :: Enctype -> Widget -> Widget
widgetForm enctype widget = [whamlet|
<body>
    <header>
        <img src="http://i62.tinypic.com/vfa1qe.png" alt="Logotipo Makeup Lovers" id="logotipo">
        <nav>
            <a href=@{HomeUserR}>Home
            <a href=@{CadastroPostR}>Novo Post
            <a href=@{ListarUserR}>Administradores
            <a href=@{ListarMsgsR}>Contatos
            <a href=@{ByeR}>Sair
    <section id="main">
            <h1>
                Cadastro de post
            <form method=post action=@{CadastroPostR} enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Postar">
    <aside>
        <img src="http://i61.tinypic.com/zvv4i1.png" alt="Quem somos" id="quemsomos"/>
        <img src="http://i61.tinypic.com/4u8bcn.png" alt="Foto Ráira Sany" title="Ráira Sany" class="fotoquemsomos"/>
        <img src="http://i60.tinypic.com/2ng6cnl.png" alt="Foto Nathália" title="Nathália" class="fotoquemsomos"/>
        <p>Nathália: 20 anos
        <p>Ráira: 22 anos
    <footer>
        Desenvolvido por Nathália Souza e Ráira Medeiros
|]

widgetFormContato :: Enctype -> Widget -> Widget
widgetFormContato enctype widget = [whamlet|
<body>
    <header>
        <img src="http://i62.tinypic.com/vfa1qe.png" alt="Logotipo Makeup Lovers" id="logotipo">
        <nav>
            <a href="/">Home
            <a href=@{QuemSomosR}>Quem somos
            <a href=@{MakeR}>Makeup
            <a href=@{ModaR}>Moda
            <a href=@{ContatoR}>Contato
    <section id="main">
            <h1>
                Entre em contato
            <form method=post action=@{ContatoR} enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Enviar">
    <aside>
        <img src="http://i61.tinypic.com/zvv4i1.png" alt="Quem somos" id="quemsomos"/>
        <img src="http://i61.tinypic.com/4u8bcn.png" alt="Foto Ráira Sany" title="Ráira Sany" class="fotoquemsomos"/>
        <img src="http://i60.tinypic.com/2ng6cnl.png" alt="Foto Nathália" title="Nathália" class="fotoquemsomos"/>
        <p>Nathália: 20 anos
        <p>Ráira: 22 anos
    <footer>
        Desenvolvido por Nathália Souza e Ráira Medeiros
|]

widgetFormLogin :: Route Sitio -> Enctype -> Widget -> Text -> Text -> Widget
widgetFormLogin x enctype widget y val = do
     msg <- getMessage
     $(whamletFile "form.hamlet")
     toWidget $(luciusFile "css.lucius")

widgetFormUser :: Enctype -> Widget -> Widget
widgetFormUser enctype widget = ([whamlet|
<body>
    <header>
        <img src="http://i62.tinypic.com/vfa1qe.png" alt="Logotipo Makeup Lovers" id="logotipo">
        <nav>
            <a href=@{HomeUserR}>Home
            <a href=@{CadastroPostR}>Novo Post
            <a href=@{ListarUserR}>Administradores
            <a href=@{ListarMsgsR}>Contatos
            <a href=@{ByeR}>Sair
    <section id="main">
       <h1>Cadastro de usuário
       <form method=post action=@{CadastroUserR} enctype=#{enctype}>
           ^{widget}
           <input type="submit" value="Cadastrar">
    <aside>
        <img src="http://i61.tinypic.com/zvv4i1.png" alt="Quem somos" id="quemsomos"/>
        <img src="http://i61.tinypic.com/4u8bcn.png" alt="Foto Ráira Sany" title="Ráira Sany" class="fotoquemsomos"/>
        <img src="http://i60.tinypic.com/2ng6cnl.png" alt="Foto Nathália" title="Nathália" class="fotoquemsomos"/>
        <p>Nathália: 20 anos
        <p>Ráira: 22 anos
   <footer>
        Desenvolvido por Nathália Souza e Ráira Medeiros
|] >> toWidget $(luciusFile "css.lucius"))

getHomeR :: Handler Html
getHomeR = do
                 listaP <- runDB $ (selectList [] [Asc PostTitulo])
                 defaultLayout ([whamlet|
<body>
    <header>
        <img src="http://i62.tinypic.com/vfa1qe.png" alt="Logotipo Makeup Lovers" id="logotipo">
        <nav>
            <a href="/">Home
            <a href=@{QuemSomosR}>Quem somos
            <a href=@{MakeR}>Makeup
            <a href=@{ModaR}>Moda
            <a href=@{ContatoR}>Contato
    <section id="main">
       <h1>Listagem de todos os posts do Blog (com bd)
           $forall Entity pid post <- listaP
             <h2> #{postTitulo post} <br>
             <p> #{postConteudo post} <br>
             <h3> Autor: #{postAutor post} <br>
             <h3> Categoria: #{postCategoria post}
    <aside>
        <img src="http://i61.tinypic.com/zvv4i1.png" alt="Quem somos" id="quemsomos"/>
        <img src="http://i61.tinypic.com/4u8bcn.png" alt="Foto Ráira Sany" title="Ráira Sany" class="fotoquemsomos"/>
        <img src="http://i60.tinypic.com/2ng6cnl.png" alt="Foto Nathália" title="Nathália" class="fotoquemsomos"/>
        <p>Nathália: 20 anos
        <p>Ráira: 22 anos
   <footer>
        Desenvolvido por Nathália Souza e Ráira Medeiros
|] >> toWidget $(luciusFile "css.lucius"))

getMakeR :: Handler Html
getMakeR = do
                 listaMake <- runDB $ (selectList [PostCategoria ==. "Make"] []) 
                 defaultLayout ([whamlet|
<body>
  <header>
    <img src="http://i62.tinypic.com/vfa1qe.png" alt="Logotipo Makeup Lovers" id="logotipo">
        <nav>
            <a href="/">Home
            <a href=@{QuemSomosR}>Quem somos
            <a href=@{MakeR}>Makeup
            <a href=@{ModaR}>Moda
            <a href=@{ContatoR}>Contato

 <section id="main">
     <h1>Posts da categoria Make
         $forall Entity pid post <- listaMake
             <h2> #{postTitulo post} <br>
             <p> #{postConteudo post} <br>
             <h3> Autor: #{postAutor post} <br>
             <h3> Categoria: #{postCategoria post}

 <aside>
     <img src="http://i61.tinypic.com/zvv4i1.png" alt="Quem somos" id="quemsomos"/>
     <img src="http://i61.tinypic.com/4u8bcn.png" alt="Foto Ráira Sany" title="Ráira Sany" class="fotoquemsomos"/>
     <img src="http://i60.tinypic.com/2ng6cnl.png" alt="Foto Nathália" title="Nathália" class="fotoquemsomos"/>
     <p>Nathália: 20 anos
     <p>Ráira: 22 anos
 <footer>
    Desenvolvido por Nathália Souza e Ráira Medeiros
|] >> toWidget $(luciusFile "css.lucius"))

getModaR :: Handler Html
getModaR = do
                 listaModa <- runDB $ (selectList [PostCategoria ==. "Moda"] []) 
                 defaultLayout ([whamlet|
<body>
  <header>
    <img src="http://i62.tinypic.com/vfa1qe.png" alt="Logotipo Makeup Lovers" id="logotipo">
        <nav>
            <a href="/">Home
            <a href=@{QuemSomosR}>Quem somos
            <a href=@{MakeR}>Makeup
            <a href=@{ModaR}>Moda
            <a href=@{ContatoR}>Contato

 <section id="main">
     <h1>Posts da categoria Moda
         $forall Entity pid post <- listaModa
             <h2> #{postTitulo post} <br>
             <p> #{postConteudo post} <br>
             <h3> Autor: #{postAutor post} <br>
             <h3> Categoria: #{postCategoria post}

 <aside>
     <img src="http://i61.tinypic.com/zvv4i1.png" alt="Quem somos" id="quemsomos"/>
     <img src="http://i61.tinypic.com/4u8bcn.png" alt="Foto Ráira Sany" title="Ráira Sany" class="fotoquemsomos"/>
     <img src="http://i60.tinypic.com/2ng6cnl.png" alt="Foto Nathália" title="Nathália" class="fotoquemsomos"/>
     <p>Nathália: 20 anos
     <p>Ráira: 22 anos
 <footer>
    Desenvolvido por Nathália Souza e Ráira Medeiros
|] >> toWidget $(luciusFile "css.lucius"))

widgetHtmlUser :: Widget
widgetHtmlUser = [whamlet|
<body>
  <header>
    <img src="http://i62.tinypic.com/vfa1qe.png" alt="Logotipo Makeup Lovers" id="logotipo">
        <nav>
            <a href=@{HomeUserR}>Home
            <a href=@{CadastroPostR}>Novo Post
            <a href=@{ListarUserR}>Administradores
            <a href=@{ListarMsgsR}>Contatos
            <a href=@{ByeR}>Sair

 <section id="main">
    <h1>Bem-vindo administrador!
 <aside>
     <img src="http://i61.tinypic.com/zvv4i1.png" alt="Quem somos" id="quemsomos"/>
     <img src="http://i61.tinypic.com/4u8bcn.png" alt="Foto Ráira Sany" title="Ráira Sany" class="fotoquemsomos"/>
     <img src="http://i60.tinypic.com/2ng6cnl.png" alt="Foto Nathália" title="Nathália" class="fotoquemsomos"/>
     <p>Nathália: 20 anos
     <p>Ráira: 22 anos
 <footer>
    Desenvolvido por Nathália Souza e Ráira Medeiros
|]

widgetHtmlQuemSomos :: Widget
widgetHtmlQuemSomos = [whamlet|
<body>
  <header>
    <img src="http://i62.tinypic.com/vfa1qe.png" alt="Logotipo Makeup Lovers" id="logotipo">
        <nav>
            <a href="/">Home
            <a href=@{QuemSomosR}>Quem somos
            <a href=@{MakeR}>Makeup
            <a href=@{ModaR}>Moda
            <a href=@{ContatoR}>Contato

 <section id="main">
    <h1>Nathália
    <p>Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book.
    <h1>Ráira
    <p>Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book.
 <aside>
     <img src="http://i61.tinypic.com/zvv4i1.png" alt="Quem somos" id="quemsomos"/>
     <img src="http://i61.tinypic.com/4u8bcn.png" alt="Foto Ráira Sany" title="Ráira Sany" class="fotoquemsomos"/>
     <img src="http://i60.tinypic.com/2ng6cnl.png" alt="Foto Nathália" title="Nathália" class="fotoquemsomos"/>
     <p>Nathália: 20 anos
     <p>Ráira: 22 anos
 <footer>
    Desenvolvido por Nathália Souza e Ráira Medeiros
|]

getListarUserR :: Handler Html
getListarUserR = do
               listaUser <- runDB $ selectList [][Asc UserLogin]
               defaultLayout ([whamlet|
<body>
  <header>
    <img src="http://i62.tinypic.com/vfa1qe.png" alt="Logotipo Makeup Lovers" id="logotipo">
        <nav>
            <a href=@{HomeUserR}>Home
            <a href=@{CadastroPostR}>Novo Post
            <a href=@{ListarUserR}>Administradores
            <a href=@{ListarMsgsR}>Contatos
            <a href=@{ByeR}>Sair

 <section id="main">
     <h1>Usuários Cadastrados:
                    $forall Entity pid user <- listaUser
                     <h2> #{userLogin user} <br>
 <aside>
     <img src="http://i61.tinypic.com/zvv4i1.png" alt="Quem somos" id="quemsomos"/>
     <img src="http://i61.tinypic.com/4u8bcn.png" alt="Foto Ráira Sany" title="Ráira Sany" class="fotoquemsomos"/>
     <img src="http://i60.tinypic.com/2ng6cnl.png" alt="Foto Nathália" title="Nathália" class="fotoquemsomos"/>
     <p>Nathália: 20 anos
     <p>Ráira: 22 anos
 <footer>
    Desenvolvido por Nathália Souza e Ráira Medeiros
|] >> toWidget $(luciusFile "css.lucius"))

getListarMsgsR :: Handler Html
getListarMsgsR = do
               listaMsgs <- runDB $ selectList [][Asc ContatoAssunto]
               defaultLayout ([whamlet|
<body>
  <header>
    <img src="http://i62.tinypic.com/vfa1qe.png" alt="Logotipo Makeup Lovers" id="logotipo">
        <nav>
            <a href=@{HomeUserR}>Home
            <a href=@{CadastroPostR}>Novo Post
            <a href=@{ListarUserR}>Administradores
            <a href=@{ListarMsgsR}>Contatos
            <a href=@{ByeR}>Sair

 <section id="main">
     <h1>Mensagens recebidas:
                    $forall Entity pid contato <- listaMsgs
                     <h2> #{contatoAssunto contato} <br>
                     <h3> #{contatoMensagem contato} <br>
 <aside>
     <img src="http://i61.tinypic.com/zvv4i1.png" alt="Quem somos" id="quemsomos"/>
     <img src="http://i61.tinypic.com/4u8bcn.png" alt="Foto Ráira Sany" title="Ráira Sany" class="fotoquemsomos"/>
     <img src="http://i60.tinypic.com/2ng6cnl.png" alt="Foto Nathália" title="Nathália" class="fotoquemsomos"/>
     <p>Nathália: 20 anos
     <p>Ráira: 22 anos
 <footer>
    Desenvolvido por Nathália Souza e Ráira Medeiros
|] >> toWidget $(luciusFile "css.lucius"))

getQuemSomosR :: Handler Html
getQuemSomosR = defaultLayout (widgetHtmlQuemSomos >> toWidget $(luciusFile "css.lucius"))

getHomeUserR :: Handler Html
getHomeUserR = defaultLayout (widgetHtmlUser >> toWidget $(luciusFile "css.lucius"))


getCadastroPostR :: Handler Html
getCadastroPostR = do
             (widget, enctype) <- generateFormPost formPost
             defaultLayout $ widgetForm enctype widget >> toWidget $(luciusFile "css.lucius")

postCadastroPostR :: Handler Html
postCadastroPostR = do
                ((result, _), _) <- runFormPost formPost
                case result of
                    FormSuccess post -> do
                        runDB $ insert post
                        defaultLayout [whamlet|
                             <h1>#{postTitulo post} Inserido com sucesso.
                        |]
                    _ -> redirect CadastroPostR

getContatoR :: Handler Html
getContatoR = do
            (widget, enctype) <- generateFormPost formContato
            defaultLayout $ widgetFormContato enctype widget >> toWidget $(luciusFile "css.lucius")

postContatoR :: Handler Html
postContatoR = do
                ((result, _), _) <- runFormPost formContato
                case result of
                    FormSuccess contato -> do
                        runDB $ insert contato
                        defaultLayout [whamlet|
                             <h1>Mensagem enviada com sucesso.
                        |]
                    _ -> redirect ContatoR

getCadastroUserR :: Handler Html
getCadastroUserR = do
            (widget, enctype) <- generateFormPost formUser
            defaultLayout $ widgetFormUser enctype widget >> toWidget $(luciusFile "css.lucius")

postCadastroUserR :: Handler Html
postCadastroUserR = do
                ((result, _), _) <- runFormPost formUser
                case result of
                    FormSuccess user -> do
                        runDB $ insert user
                        defaultLayout [whamlet|
                             <h1>#{userLogin user} inserido com sucesso.
                        |]
                    _ -> redirect CadastroUserR

getLoginR :: Handler Html
getLoginR = do
    (wid,enc) <- generateFormPost formUser
    defaultLayout $ widgetFormLogin LoginR enc wid "" "Login"

postLoginR :: Handler Html
postLoginR = do
    ((result,_),_) <- runFormPost formUser
    case result of
        FormSuccess usr -> do
            usuario <- runDB $ selectFirst [UserLogin ==. userLogin usr, UserSenha ==. userSenha usr ] []
            case usuario of
                Just (Entity uid usr) -> do
                    setSession "_ID" (userLogin usr)
                    redirect HomeUserR
                Nothing -> do
                    setMessage $ [shamlet| Invalid user |]
                    redirect LoginR
        _ -> redirect LoginR

getByeR :: Handler Html
getByeR = do
    deleteSession "_ID"
    defaultLayout [whamlet| BYE! |]

connStr = "dbname=denn8aphhsbks6 host=ec2-107-22-187-89.compute-1.amazonaws.com user=ylsbjsmhcifelq password=oeu_XbERka9NuUhhC8x3KVXKlO"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
       runSqlPersistMPool (runMigration migrateAll) pool
       warpEnv (Sitio pool)