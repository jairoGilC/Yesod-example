{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

-- { "id": 1, "fieldOne": "example", "fieldTwo": 1, "fieldThree": true }
instance ToJSON (Entity Demo) where
    toJSON (Entity demoId demo) = object
        [ "id"         .= (String $ toPathPiece demoId)
        , "fieldOne"   .= demoFieldOne demo
        , "fieldTwo"   .= demoFieldTwo demo
        , "fieldThree" .= demoFieldThree demo
        ]

instance FromJSON Demo where
    parseJSON (Object demo) = Demo
        <$> demo .: "fieldOne"
        <*> demo .: "fieldTwo"
        <*> demo .: "fieldThree"

    parseJSON _ = mzero

data Privileges =
  PrvDemoOne         -- ^ what can be demo one...
  | PrvDemoTwo       -- ^ what can be demo two...
  deriving (Show,Read,Eq)

derivePersistField "Privileges"