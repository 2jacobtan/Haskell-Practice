module DefiningSchemeFunctions where

import Types
import IOPrimitives
import VarsAndAssignment (nullEnv,  bindVars, liftThrows)
import Evaluation (primitives, eval)

primitiveBindings :: IO Env
primitiveBindings =
  ( $
      map (makeFunc IOFunc) ioPrimitives
        ++ map (makeFunc PrimitiveFunc) primitives
  )
    . bindVars
    =<< nullEnv
  where
    makeFunc constructor (var, func) = (var, constructor func)
