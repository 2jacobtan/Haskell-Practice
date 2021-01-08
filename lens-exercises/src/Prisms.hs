{-# LANGUAGE QuasiQuotes #-}

module Prisms where

import Data.Aeson.QQ
import Data.Aeson (Value)

user1 = [aesonQQ|
  {
    "name": "qiao.yifan",
    "email": "qyifan@xingxin.com"
  }
|]

user2 = [aesonQQ|
  {
    "name": "ye.xiu",
    "metadata": {
      "num_logins": 27
    }
  }
|]

