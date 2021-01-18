{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module FancierOperators where

import Prisms hiding (user1, (<<>~), (<<<>~))

import Data.Text

data User = User
  { _name     :: Text
  , _userid   :: Int
  , _metadata :: UserInfo
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
  }

users = [aesonQQ|
  {
    "users": [
      {
        "name": "qiao.yifan",
        "email": "qyifan@xingxin.com",
        "metadata": {
          "num_logins": 5
        }
      },
      {
        "name": "ye.xiu",
        "metadata": {
          "num_logins": 27,
          "associated_ips": [
            "52.49.1.233",
            "52.49.1.234"
          ]
        }
      },
      {
        "name": "su.mucheng",
        "email": "smucheng@xingxin.com",
        "metadata": {
          "associated_ips": [
            "51.2.244.193"
          ]
        }
      }
    ]
  }
|]

p1e0 = user1 & metadata.associatedIPs <<<>~ ["127.0.0.1"]
-- >>> p1e0
-- (["52.39.193.61","52.39.193.75"],User {_name = "qiao.yifan", _userid = 103, _metadata = UserInfo {_numLogins = 20, _associatedIPs = ["52.39.193.61","52.39.193.75","127.0.0.1"]}})

p1e1 = user1 & metadata.numLogins +~ 1
-- >>> p1e1
-- User {_name = "qiao.yifan", _userid = 103, _metadata = UserInfo {_numLogins = 21, _associatedIPs = ["52.39.193.61","52.39.193.75"]}}

p1e2 = users & key "users".values.key "name"._String <>~ ".test"
-- >>> p1e2
-- Object (fromList [("users",Array [Object (fromList [("email",String "qyifan@xingxin.com"),("name",String "qiao.yifan.test"),("metadata",Object (fromList [("num_logins",Number 5.0)]))]),Object (fromList [("name",String "ye.xiu.test"),("metadata",Object (fromList [("associated_ips",Array [String "52.49.1.233",String "52.49.1.234"]),("num_logins",Number 27.0)]))]),Object (fromList [("email",String "smucheng@xingxin.com"),("name",String "su.mucheng.test"),("metadata",Object (fromList [("associated_ips",Array [String "51.2.244.193"])]))])])])

p1e3 = users & key "users".values.key "name"._String <<>~ ".test"
-- >>> p1e3
-- ("qiao.yifan.testye.xiu.testsu.mucheng.test",Object (fromList [("users",Array [Object (fromList [("email",String "qyifan@xingxin.com"),("name",String "qiao.yifan.test"),("metadata",Object (fromList [("num_logins",Number 5.0)]))]),Object (fromList [("name",String "ye.xiu.test"),("metadata",Object (fromList [("associated_ips",Array [String "52.49.1.233",String "52.49.1.234"]),("num_logins",Number 27.0)]))]),Object (fromList [("email",String "smucheng@xingxin.com"),("name",String "su.mucheng.test"),("metadata",Object (fromList [("associated_ips",Array [String "51.2.244.193"])]))])])]))

p1e4 = user1 & userid ^~ 2
-- >>> p1e4
-- User {_name = "qiao.yifan", _userid = 10609, _metadata = UserInfo {_numLogins = 20, _associatedIPs = ["52.39.193.61","52.39.193.75"]}}

p2e1 = users & key "users".values.key "name"._String <>~ ".dev"
-- >>> p2e1
-- Object (fromList [("users",Array [Object (fromList [("email",String "qyifan@xingxin.com"),("name",String "qiao.yifan.dev"),("metadata",Object (fromList [("num_logins",Number 5.0)]))]),Object (fromList [("name",String "ye.xiu.dev"),("metadata",Object (fromList [("associated_ips",Array [String "52.49.1.233",String "52.49.1.234"]),("num_logins",Number 27.0)]))]),Object (fromList [("email",String "smucheng@xingxin.com"),("name",String "su.mucheng.dev"),("metadata",Object (fromList [("associated_ips",Array [String "51.2.244.193"])]))])])])

p2e2 = user1 & metadata.numLogins %~ (`div` 2)
-- >>> p2e2
-- User {_name = "qiao.yifan", _userid = 103, _metadata = UserInfo {_numLogins = 10, _associatedIPs = ["52.39.193.61","52.39.193.75"]}}

p2e3 = users & key "users".values.key "metadata".key "num_logins"._Integer +~ 1
-- >>> p2e3
-- Object (fromList [("users",Array [Object (fromList [("email",String "qyifan@xingxin.com"),("name",String "qiao.yifan"),("metadata",Object (fromList [("num_logins",Number 6.0)]))]),Object (fromList [("name",String "ye.xiu"),("metadata",Object (fromList [("associated_ips",Array [String "52.49.1.233",String "52.49.1.234"]),("num_logins",Number 28.0)]))]),Object (fromList [("email",String "smucheng@xingxin.com"),("name",String "su.mucheng"),("metadata",Object (fromList [("associated_ips",Array [String "51.2.244.193"])]))])])])

p2e4 = user1 & userid *~ 4
-- >>> p2e4
-- User {_name = "qiao.yifan", _userid = 412, _metadata = UserInfo {_numLogins = 20, _associatedIPs = ["52.39.193.61","52.39.193.75"]}}


-- Part 3

infixr 4 <<>~
(<<>~) :: Monoid a
      => ((a -> (a, a)) -> s -> (a, t))
      -> a
      -> s
      -> (a, t)
(<<>~) l a' = l (\a0 -> let a1 = a0 <> a' in (a1, a1))

infixr 4 <<<>~
(<<<>~) :: Monoid a
        => ((a -> (a, a)) -> s -> (a, t))
        -> a
        -> s
        -> (a, t)
(<<<>~) l a' = l (\a0 -> (a0, a0 <> a'))
