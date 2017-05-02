{-# LANGUAGE TemplateHaskell #-}

module Pardis.Distributed.TH
  ( mkProcs
  ) where

import Control.Distributed.Process.Closure
import Control.Distributed.Process.Internal.Types
import Control.Monad
import Language.Haskell.TH

-- | Template Haskell function that takes the name of a pure function and
-- generates Cloud Haskell processes from it.
--
-- Note that we can only do this for monomorph functions because we have to know
-- the result type of the functions when we use them in closures.
mkProcs :: [Name] -> DecsQ
mkProcs names = do
  procs <- forM names $ \name -> do
    -- figure out the type of the function with the given name
    typ <- do
      info <- reify name
      case info of
        VarI _ typ _ -> return typ
        _            -> fail $ show name ++ " not found"

    -- check if the function is monomorph
    let (typVars, typ') = case typ of
          ForallT vars [] mono -> fail $ show name ++ " is not monomorph"
          _                    -> ([], typ)

    -- check if the function's type if of the right form.
    (stype, ttype) <- case (typVars, typ') of
          ([], ArrowT `AppT` s `AppT` t) -> return (s, t)
          _                              -> fail $ show name ++ " has unexpected type (we need something like a -> b)"

    -- generate a new name for the process
    let procName = mkName $ nameBase name ++ "P"

    -- define the process by simply turning it into a monadic computation in
    -- Cloud Haskell's Process monad
    sequence [ sigD procName [t| $(return stype) -> Process $(return ttype) |]
             , funD procName [ clause [] (normalB [| return . $(varE name) |]) [] ]
             ]

  return (concat procs)
