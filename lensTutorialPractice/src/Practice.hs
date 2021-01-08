{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE DeriveTraversable #-}

-- atom.hs

module Practice where

import Control.Lens hiding (element)
import Control.Lens.TH
import Data.Monoid (Sum(Sum))

data Atom = Atom { _element :: String, _point :: Point } deriving (Show)

data Point = Point { _x :: Double, _y :: Double } deriving (Show)

$(makeLenses ''Atom)
$(makeLenses ''Point)

shiftAtomX :: Atom -> Atom
shiftAtomX = over (point . x) (+ 1)

atom = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }

-- >>> shiftAtomX atom
-- Atom {_element = "C", _point = Point {_x = 2.0, _y = 2.0}}

newtype Molecule = Molecule { _atoms :: [Atom] } deriving (Show)

$(makeLenses ''Molecule)

shiftMoleculeX :: Molecule -> Molecule
shiftMoleculeX = over (atoms . traverse . point . x) (+ 1)

atom1 = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }
atom2 = Atom { _element = "O", _point = Point { _x = 3.0, _y = 4.0 } }
molecule = Molecule { _atoms = [atom1, atom2] }

-- >>> shiftMoleculeX molecule
-- Molecule {_atoms = [Atom {_element = "C", _point = Point {_x = 2.0, _y = 2.0}},Atom {_element = "O", _point = Point {_x = 4.0, _y = 4.0}}]}

-- >>> view (point . x) atom
-- 1.0

type Lens' a b = forall f . Functor f => (b -> f b) -> (a -> f a)

type Getting a b  = (b -> Const b b) -> (a -> Const b a)

point' :: (Functor f) => (Point -> f Point) -> Atom -> f Atom
point' k atom@Atom{..} = fmap (\newPoint@Point{..} -> atom { _point = newPoint { _x = _x * 3 }  }) (k _point)
-- point' k atom = fmap (\newPoint -> atom { _point = newPoint }) (k (_point atom))

shiftMoleculeX' :: Molecule -> Molecule
shiftMoleculeX' = over (atoms . traverse . point' . x) (+ 1)

-- >>> shiftMoleculeX' molecule
-- Molecule {_atoms = [Atom {_element = "C", _point = Point {_x = 6.0, _y = 2.0}},Atom {_element = "O", _point = Point {_x = 12.0, _y = 4.0}}]}


data Pair a = Pair a a deriving (Functor, Foldable, Traversable, Show)

-- >>> view (atoms . traverse . point . x) molecule
-- No instance for (Monoid Double) arising from a use of ‘traverse’

-- >>> view (atoms . traverse . element) molecule
-- "CO"


