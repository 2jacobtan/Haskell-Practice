{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Lens where

import Control.Lens hiding ((.~), (%~), (^.))
import Data.Text ( Text, toUpper )

data User = User
  { _name     :: Text
  , _userid   :: Int
  , _metadata :: UserInfo
  , _email :: Text
  }
  deriving (Show)

data UserInfo = UserInfo
  { _numLogins     :: Int
  , _associatedIPs :: [Text]
  }
  deriving (Show)

makeLenses ''User
makeLenses ''UserInfo

user1 = User
  { _name = "qiao.yifan"
  , _userid = 103
  , _metadata = UserInfo
    { _numLogins = 20
    , _associatedIPs =
      [ "52.39.193.61"
      , "52.39.193.75"
      ]
    }
  , _email = "null@null.com"
  }

-- Part 1

-- >>> user1 ^. name
-- "qiao.yifan"

-- >>> user1 ^. metadata.numLogins
-- 20

_ = user1 & metadata.numLogins .~ 0
-- >>> user1 & metadata.numLogins .~ 0
-- User {_name = "qiao.yifan", _userid = 103, _metadata = UserInfo {_numLogins = 0, _associatedIPs = ["52.39.193.61","52.39.193.75"]}}

-- >>> user1 & metadata.associatedIPs %~ ("192.168.0.2" :)
-- User {_name = "qiao.yifan", _userid = 103, _metadata = UserInfo {_numLogins = 20, _associatedIPs = ["192.168.0.2","52.39.193.61","52.39.193.75"]}}

_ = metadata.numLogins %~ (+ 1) $ user1

-- >>> metadata.numLogins %~ (+ 1) $ user1
-- User {_name = "qiao.yifan", _userid = 103, _metadata = UserInfo {_numLogins = 21, _associatedIPs = ["52.39.193.61","52.39.193.75"]}}


-- Part 2

-- >>> user1 & email .~ "qyifan@xingxin.com"
-- User {_name = "qiao.yifan", _userid = 103, _metadata = UserInfo {_numLogins = 20, _associatedIPs = ["52.39.193.61","52.39.193.75"]}, _email = "qyifan@xingxin.com"}

-- >>> user1 & metadata .~ (UserInfo 17 [])
-- User {_name = "qiao.yifan", _userid = 103, _metadata = UserInfo {_numLogins = 17, _associatedIPs = []}}

-- >>> userid .~ -1 $ user1
-- User {_name = "qiao.yifan", _userid = -1, _metadata = UserInfo {_numLogins = 20, _associatedIPs = ["52.39.193.61","52.39.193.75"]}, _email = "null@null.com"}

-- >>> metadata.associatedIPs .~ [ "50.193.0.23" ] $ user1
-- User {_name = "qiao.yifan", _userid = 103, _metadata = UserInfo {_numLogins = 20, _associatedIPs = ["50.193.0.23"]}, _email = "null@null.com"}

-- >>> user1 ^. metadata.numLogins
-- 20


-- Part 3
-- >>> user1 ^. metadata.associatedIPs
-- ["52.39.193.61","52.39.193.75"]

-- >>> user1 & metadata.associatedIPs %~ reverse
-- User {_name = "qiao.yifan", _userid = 103, _metadata = UserInfo {_numLogins = 20, _associatedIPs = ["52.39.193.75","52.39.193.61"]}, _email = "null@null.com"}

-- >>> user1 & name %~ toUpper
-- User {_name = "QIAO.YIFAN", _userid = 103, _metadata = UserInfo {_numLogins = 20, _associatedIPs = ["52.39.193.61","52.39.193.75"]}, _email = "null@null.com"}

-- >>> user1 & metadata.numLogins .~ 1
-- User {_name = "qiao.yifan", _userid = 103, _metadata = UserInfo {_numLogins = 1, _associatedIPs = ["52.39.193.61","52.39.193.75"]}, _email = "null@null.com"}

-- >>> user1 & metadata.associatedIPs %~ take 1
-- User {_name = "qiao.yifan", _userid = 103, _metadata = UserInfo {_numLogins = 20, _associatedIPs = ["52.39.193.61"]}, _email = "null@null.com"}


-- Part 4

infixr 4 .~
(.~) :: ((a -> Identity b) -> s -> Identity t)
     -> b
     -> s
     -> t
l .~ b = runIdentity . l (const $ Identity b) 

infixr 4 %~
(%~) :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
l %~ fab = runIdentity . l (Identity . fab)

infixl 8 ^.
(^.) :: s
     -> ((a -> Const a b) -> s -> Const a t)
     -> a
s ^. l = getConst $ l Const s

name' :: Functor f => (Text -> f Text) -> User -> f User
name' afb user@User{_name} = afb _name <&> (\x -> user{_name = x})

-- >>> user1 ^. name
-- "qiao.yifan"

-- >>> user1 & name' %~ toUpper
-- User {_name = "QIAO.YIFAN", _userid = 103, _metadata = UserInfo {_numLogins = 20, _associatedIPs = ["52.39.193.61","52.39.193.75"]}, _email = "null@null.com"}

