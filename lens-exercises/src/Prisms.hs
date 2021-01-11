{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Prisms where

import Data.Aeson.QQ
import Data.Aeson (Value)
import Data.Aeson.Lens
import qualified Data.Text as Text
import Control.Lens hiding ((.~), (%~), (^.), (^?), _Just)
import Lens ((.~), (%~), (^.))
import Data.Semigroup (First(getFirst, First))

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

_= user1 ^? key "email"._String
-- >>> user1 ^? key "email"._String
-- Just "qyifan@xingxin.com"

-- >>> user2 ^? key "email"._String
-- Nothing

-- >>> user2 ^. key "email"._String
-- ""

-- >>> user2 ^? key "metadata".key "associated_ips"._Array
-- Nothing

-- >>> user2 ^. key "metadata".key "associated_ips"._Array
-- []

-- >>> user1 ^? key "name"._String.to Text.toUpper
-- Just "QIAO.YIFAN"

-- >>> user1 & key "name"._String .~ "su.mucheng"
-- Object (fromList [("email",String "qyifan@xingxin.com"),("name",String "su.mucheng")])

-- >>> user2 & key "email"._String .~ "yxiu@xingxin.com"
-- Object (fromList [("name",String "ye.xiu"),("metadata",Object (fromList [("num_logins",Number 27.0)]))])

-- >>>    user2 & key "name"._String %~ Text.reverse
-- Object (fromList [("name",String "uix.ey"),("metadata",Object (fromList [("num_logins",Number 27.0)]))])


-- Part 2

-- >>> user2 ^? key "metadata".key "num_logins"._Integer
-- Just 27

-- >>> user1 & key "metadata".key "num_logins"._Integer .~ 25
-- Object (fromList [("email",String "qyifan@xingxin.com"),("name",String "qiao.yifan")])

-- >>> user2 & key "metadata".key "num_logins"._Integer %~ (+ 1)
-- Object (fromList [("name",String "ye.xiu"),("metadata",Object (fromList [("num_logins",Number 28.0)]))])

-- >>> user1 ^. key "email"._String
-- "qyifan@xingxin.com"

-- >>> user2 & key "name"._String .~ "50"
-- Object (fromList [("name",String "50"),("metadata",Object (fromList [("num_logins",Number 27.0)]))])

_= user2 & key "name"._String .~ "50"

type First' a = Maybe (First a)
getFirst' (Just (First x)) = Just x
getFirst' Nothing = Nothing

infixl 8 ^?
(^?) :: s
     -> ((a -> Const (First' a) b) -> s -> Const (First' a) t)
     -> Maybe a
s ^? l = fmap getFirst . getConst $ l (Const . Just . First) s

_Just :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
_Just = traverse

aMaybe = Just "a"
noMaybe = Nothing :: Maybe String
-- >>> aMaybe ^? _Just
-- >>> aMaybe ^. _Just
-- >>> aMaybe & _Just %~ (++ "b")
-- >>> aMaybe & _Just .~ ("b")
-- Just "a"

-- "a"

-- Just "ab"

-- Just "b"

-- >>> noMaybe ^. _Just
-- >>> noMaybe ^? _Just
-- >>> noMaybe & _Just %~ (++ "b")
-- >>> noMaybe & _Just .~ ("b")
-- ""

-- Nothing

-- Nothing

-- Nothing



