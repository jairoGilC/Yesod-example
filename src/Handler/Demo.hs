{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Demo where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

--Aform From Entity Demo
demoForm :: Maybe Demo -> AForm Handler Demo
demoForm   demo = Demo 
                <$> areq textField "fieldone" (demoFieldOne <$> demo)
                <*> areq intField "fieldTwo" (demoFieldTwo <$> demo) 
                <*> areq boolField "fieldThree" (demoFieldThree <$> demo) 


--CRUD 
--Create
getDemoNewR ::  Handler Html 
getDemoNewR = do
           (widget, encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ demoForm Nothing
           defaultLayout $ do
                let actionR = DemoNewR
                $(widgetFile "Demo/DemoCreate")

postDemoNewR :: Handler Html
postDemoNewR = do
                ((result,widget), encoding) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ demoForm  Nothing
                case result of
                     FormSuccess demo -> do 
                                 _ <- runDB $ insert demo
                                 redirect HomeR
                     _ -> defaultLayout $ do 
                        let actionR = DemoNewR
                        $(widgetFile "Demo/DemoCreate")


getDemoSearchR ::  Handler Html 
getDemoSearchR = do
           (widget, encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ demoForm Nothing
           defaultLayout $ do
                let actionR = DemoSearchR
                $(widgetFile "Demo/DemoSearch")

postDemoSearchR :: Handler Html
postDemoSearchR = do
                ((result,widget), encoding) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ demoForm  Nothing
                case result of
                     FormSuccess _ -> do 
                                 demos <- runDB $ selectList [] []
                                 defaultLayout $ do
                                    $(widgetFile "Demo/DemoList")
                     _ -> defaultLayout $ do 
                        let actionR = DemoSearchR
                        $(widgetFile "Demo/DemoCreate")


--JSON SERVICES

getDemosJsonR :: Handler Value 
getDemosJsonR = do
    demos <- runDB $ selectList [] [] :: Handler [Entity Demo]

    return $ object ["demos" .= demos]

postDemosJsonR :: Handler Value 
postDemosJsonR = do
    demo <- requireJsonBody :: Handler Demo
    _    <- runDB $ insert demo

    sendResponseStatus status201 ("CREATED" :: Text)

getDemoJsonR :: DemoId -> Handler Value 
getDemoJsonR demoId = do
    demo <- runDB $ get404 demoId

    return $ object ["demo" .= (Entity demoId demo)]

putDemoJsonR :: DemoId -> Handler Value 
putDemoJsonR demoId = do
    demo <- requireJsonBody :: Handler Demo

    runDB $ replace demoId demo

    sendResponseStatus status200 ("UPDATED" :: Text)

deleteDemoJsonR :: DemoId -> Handler Value 
deleteDemoJsonR demoId = do
    runDB $ delete demoId

    sendResponseStatus status200 ("DELETED" :: Text)






















