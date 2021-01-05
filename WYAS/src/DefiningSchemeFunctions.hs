module DefiningSchemeFunctions where

import Types
import VarsAndAssignment (nullEnv,  bindVars, liftThrows)
import Evaluation (primitives, eval)

primitiveBindings :: IO Env
primitiveBindings = ($ map makePrimitiveFunc primitives) . bindVars =<< nullEnv 
-- primitiveBindings = nullEnv >>= flip bindVars (map makePrimitiveFunc primitives)
  where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)
