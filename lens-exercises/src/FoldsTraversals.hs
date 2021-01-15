{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module FoldsTraversals where

import Prisms hiding ((^..), sequenceAOf, traverseOf, traversed)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Monoid
import Data.IORef
import Data.Functor (($>))
import Data.Text (Text)
import Data.Maybe (isJust)
import Data.Vector (Vector, fromList)

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

_=users ^.. key "users".values.key "name"._String

-- >>> users ^.. key "users".values.key "name"._String
-- >>> users ^.. key "users".values.key "email"._String
-- >>> users ^.. key "users"._Array.traversed.key "email"._String
-- >>> users ^.. key "users"._Array.folded.key "email"._String
-- ["qiao.yifan","ye.xiu","su.mucheng"]

-- ["qyifan@xingxin.com","smucheng@xingxin.com"]

-- ["qyifan@xingxin.com","smucheng@xingxin.com"]

-- ["qyifan@xingxin.com","smucheng@xingxin.com"]


_= users ^.. key "users"._Array.folded.key "email"._String

-- >>> users ^. key "users".values.key "name"._String
-- "qiao.yifanye.xiusu.mucheng"

_= users ^. key "users".values.key "name"._String

-- >>> users & key "users".values.key "name"._String %~ Text.toUpper
-- Object (fromList [("users",Array [Object (fromList [("email",String "qyifan@xingxin.com"),("name",String "QIAO.YIFAN"),("metadata",Object (fromList [("num_logins",Number 5.0)]))]),Object (fromList [("name",String "YE.XIU"),("metadata",Object (fromList [("associated_ips",Array [String "52.49.1.233",String "52.49.1.234"]),("num_logins",Number 27.0)]))]),Object (fromList [("email",String "smucheng@xingxin.com"),("name",String "SU.MUCHENG"),("metadata",Object (fromList [("associated_ips",Array [String "51.2.244.193"])]))])])])

-- >>> users ^.. key "users".values.key "metadata".key "associated_ips"._Array
-- >>> users ^.. key "users".values.key "metadata".key "associated_ips".values._String
-- [[String "52.49.1.233",String "52.49.1.234"],[String "51.2.244.193"]]

-- ["52.49.1.233","52.49.1.234","51.2.244.193"]

-- >>> users & foldlOf' (key "users".values.key "metadata".key "num_logins"._Integer) (+) 0
-- 32

ex9 = users &
  foldMapOf
    (key "users"._Array.folded.key "name"._String)
    (\x -> Any $ Text.length x <= 8)
-- >>> ex9
-- Any {getAny = True}


-- Part 2

p2ex0 = users &
  traverseOf
    (key "users".values.key "name"._String)
    (\x -> print x *> fmap Text.pack getLine)

dop2ex0 = p2ex0 >>= print

p2ex1 = do
  ref <- newIORef 0
  result <- users &
    traverseOf
      (key "users"
        ._Array
        .traversed
        .key "metadata"
        .key "num_logins"
        ._Integer)
      (\x -> modifyIORef' ref (+x) *> readIORef ref)
  print =<< readIORef ref
  return result

dop2ex1 = p2ex1 >>= print -- redundant on GHCi; just use p2ex1

p2ex2 = print =<<
  (users &
    traverseOf
      (key "users".values.key "email"._String)
      (\x -> Text.putStrLn x $> Text.reverse x)
    )
  
getAliasMay :: Text -> Maybe Text
getAliasMay "ye.xiu" = Just "ye.qiu"
getAliasMay _        = Nothing

getAliasMay' :: Text -> Maybe Text
getAliasMay' "ye.xiu" = Just "ye.qiu"
getAliasMay' x = Just x

p2ex3 = users &
  traverseOf
    (key "users".values.key "name"._String)
    getAliasMay
-- >>> p2ex3
-- Nothing

p2ex4 = users &
  traverseOf
    (key "users".values.key "name"._String)
    getAliasMay'
-- >>> p2ex4
-- Just (Object (fromList [("users",Array [Object (fromList [("email",String "qyifan@xingxin.com"),("name",String "qiao.yifan"),("metadata",Object (fromList [("num_logins",Number 5.0)]))]),Object (fromList [("name",String "ye.qiu"),("metadata",Object (fromList [("associated_ips",Array [String "52.49.1.233",String "52.49.1.234"]),("num_logins",Number 27.0)]))]),Object (fromList [("email",String "smucheng@xingxin.com"),("name",String "su.mucheng"),("metadata",Object (fromList [("associated_ips",Array [String "51.2.244.193"])]))])])]))


-- Part 3

p3e1 = users ^.. key "users".values.key "name"._String & filter (isJust . Text.find (=='u'))
p3e1b = users & foldrOf (key "users".values.key "name"._String) (\x r -> if isJust . Text.find (=='u') $ x then x:r else r) []
p3e1c = users ^.. key "users".values.key "name"._String.filtered (isJust . Text.find (=='u'))
-- >>> p3e1
-- >>> p3e1b
-- >>> p3e1c
-- ["ye.xiu","su.mucheng"]

-- ["ye.xiu","su.mucheng"]

-- ["ye.xiu","su.mucheng"]

p3e2 = users ^.. key "users".values.key "metadata".key "associated_ips".values.filtered (== "51.2.244.193")
p3e2b = users & foldMapOf (key "users".values.key "metadata".key "associated_ips".values)(Any . (== "51.2.244.193")) & getAny
-- >>> p3e2
-- >>> p3e2b
-- [String "51.2.244.193"]

-- True

p3e3 = users & traverseOf_ (key "users".values.key "metadata".key "associated_ips".values._String) Text.putStrLn

p3e4 = users & foldMapOf (key "users".values.key "metadata".key "num_logins"._Integer) Sum & getSum
-- >>> p3e4
-- 32

p3e5 = do
  prefix <- putStr "Enter prefix: " >> getLine <&> Text.pack
  print =<< (users & traverseOf (key "users".values.key "name"._String) (pure . Text.append prefix))


-- Part 5

p4e1' :: [Vector Value]
p4e1' = users &
  flip (^..)
    (key "users".values.key "metadata".key "associated_ips"._Array)

p4e1a :: Vector ()
p4e1a = users &
  sequenceAOf_
    (key "users".values.key "metadata".key "associated_ips"._Array)
-- >>> p4e1'
-- >>> p4e1a
-- [[String "52.49.1.233",String "52.49.1.234"],[String "51.2.244.193"]]

-- [(),()]

-- !!! I can't get this to work
-- p4e1b :: Vector Value
-- p4e1b = users &
--   sequenceAOf
--     (key "users".values.key "metadata".key "associated_ips"._Array)
--     --- ^ type won't fit, even though it works for sequenceAOf_ and flip (^..) ---- see prior paragraphs


-- p4e2err = users &
--   key "users"._Array.folded.key "name"._String .~ "<unknown>"
p4e2a = users &
  key "users"._Array.traversed.key "name"._String .~ "<unknown>"
p4e2b = users &
  key "users".values.key "name"._String .~ "<unknown>"
-- >>> p4e2a
-- >>> p4e2b
-- Object (fromList [("users",Array [Object (fromList [("email",String "qyifan@xingxin.com"),("name",String "<unknown>"),("metadata",Object (fromList [("num_logins",Number 5.0)]))]),Object (fromList [("name",String "<unknown>"),("metadata",Object (fromList [("associated_ips",Array [String "52.49.1.233",String "52.49.1.234"]),("num_logins",Number 27.0)]))]),Object (fromList [("email",String "smucheng@xingxin.com"),("name",String "<unknown>"),("metadata",Object (fromList [("associated_ips",Array [String "51.2.244.193"])]))])])])

-- Object (fromList [("users",Array [Object (fromList [("email",String "qyifan@xingxin.com"),("name",String "<unknown>"),("metadata",Object (fromList [("num_logins",Number 5.0)]))]),Object (fromList [("name",String "<unknown>"),("metadata",Object (fromList [("associated_ips",Array [String "52.49.1.233",String "52.49.1.234"]),("num_logins",Number 27.0)]))]),Object (fromList [("email",String "smucheng@xingxin.com"),("name",String "<unknown>"),("metadata",Object (fromList [("associated_ips",Array [String "51.2.244.193"])]))])])])


p4e3 = users &
  key "users"._Array.traversed.key "name"._String .~ "<unknown>"
-- >>> p4e3

p4e4 = users & key "users".values.key "email"._String %~ (<> ".cn")
-- >>> p4e4
-- Object (fromList [("users",Array [Object (fromList [("email",String "qyifan@xingxin.com.cn"),("name",String "qiao.yifan"),("metadata",Object (fromList [("num_logins",Number 5.0)]))]),Object (fromList [("name",String "ye.xiu"),("metadata",Object (fromList [("associated_ips",Array [String "52.49.1.233",String "52.49.1.234"]),("num_logins",Number 27.0)]))]),Object (fromList [("email",String "smucheng@xingxin.com.cn"),("name",String "su.mucheng"),("metadata",Object (fromList [("associated_ips",Array [String "51.2.244.193"])]))])])])

p4e5 = users &
  foldMapOf
    (key "users".values.key "metadata".key "num_logins"._Integer)
    (\x -> All $ x > 1)
-- >>> p4e5
-- All {getAll = True}

p4e6 = users ^.. key "users"._Array.traversed.key "metadata"
-- >>> p4e6
-- [Object (fromList [("num_logins",Number 5.0)]),Object (fromList [("associated_ips",Array [String "52.49.1.233",String "52.49.1.234"]),("num_logins",Number 27.0)]),Object (fromList [("associated_ips",Array [String "51.2.244.193"])])]


-- Part 5

infixl 8 ^..
(^..) :: s
      -> ((a -> Const (Endo [a]) b) -> s -> Const (Endo [a]) t)
      -> [a]
s ^.. l = l (Const . Endo . ((++) . pure)) s & getConst & flip appEndo []

sequenceAOf :: ((f b -> f b) -> s -> f t) -> s -> f t
sequenceAOf l = l id

-- >>> sequenceAOf (both.traversed) ([[1,2],[3,4]],[[5,6]])
-- >>> Prisms.sequenceAOf (both.traversed) ([[1,2],[3,4]],[[5,6]])
-- [([1,3],[5]),([1,3],[6]),([1,4],[5]),([1,4],[6]),([2,3],[5]),([2,3],[6]),([2,4],[5]),([2,4],[6])]

-- [([1,3],[5]),([1,3],[6]),([1,4],[5]),([1,4],[6]),([2,3],[5]),([2,3],[6]),([2,4],[5]),([2,4],[6])]

traverseOf :: ((a -> f b) -> s -> f t) -> (a -> f b) -> s -> f t
traverseOf = id

traversed :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
traversed = traverse

-- >>>  sequenceAOf traversed $ fromList [fromList [1,2], fromList [3,4]]
-- [[1,3],[1,4],[2,3],[2,4]]


p5e5 = users ^? key "users".values.filtered (\x -> x ^? key "name" == Just "qiao.yifan").key "metadata".key "num_logins"
p5e5b = users ^? key "users".values.filtered (\x -> x ^? key "name" == Just "qiao.yifan").key "metadata".key "associated_ips"
p5e5c = users ^? key "users".values.filtered (\x -> x ^? key "name" == Just "ye.xiu").key "metadata".key "associated_ips"
-- >>> p5e5
-- >>> p5e5b
-- >>> p5e5c
-- Just (Number 5.0)

-- Nothing

-- Just (Array [String "52.49.1.233",String "52.49.1.234"])

