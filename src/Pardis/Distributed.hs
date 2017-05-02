{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module Pardis.Distributed
  ( module Pardis.Distributed
  ) where

import Control.Distributed.Process              as Pardis.Distributed
import Control.Distributed.Process.Serializable as Pardis.Distributed
import Control.Distributed.Process.Closure      as Pardis.Distributed
import Control.Concurrent.MVar
import Language.Haskell.TH
import Pardis                                   as Pardis.Distributed
import Pardis.Distributed.TH                    as Pardis.Distributed

-- | Distributed implementation of Pardis using Cloud Haskell and its Process
-- monad.
--
-- Basic processes contain a closure generator and a value of type
-- Static (SerializableDict b), i.e., a compile time constant that describes
-- how to serialise values of type b.
--
-- Execution environments contain an action getNode that is used to retrieve a
-- node on which closures are executed.
--
-- Basic processes are executed by generating a closure using the process'
-- intrinsic function and its input value. The closure is then executed on a
-- local or remote node that is fetched using getNode.
--
-- Parallel processes are executed using a lightweight local helper thread.
-- The results of the sub-processes are fully evaluated implicitly for
-- serialisation before they are sent back to the local node.
instance Pardis Process where
  data Basic Process a b = Serializable b => Basic (a -> Closure (Process b)) (Static (SerializableDict b))
  data Env   Process     = Env { getNode :: Process NodeId }

  runBasic env (Basic f dict) x = getNode env >>= \n -> call dict n (f x)

  runParallel env r p q x = do
    mvar <- liftIO newEmptyMVar
    spawnLocal $ runProcess env p x >>= liftIO . putMVar mvar
    qx   <- runProcess env q x
    px   <- liftIO $ takeMVar mvar
    runProcess env r (px, qx)

-- | Helper function to lift Cloud Haskell processes into Pardis processes.
mkBasic :: Serializable b => (a -> Closure (Process b)) -> Static (SerializableDict b) -> Proc (Basic Process) a b
mkBasic fun dict = Lifted $ Basic fun dict
{-# INLINABLE mkBasic #-}

-- | Template Haskell function that lifts Cloud Haskell processes into Pardis
-- processes. This function uses generates closures generators and selects the
-- proper static dictionaries for serialisation of results.
--
-- Note that this function will only work on names that have been passed to
-- Cloud Haskell's remotable function which takes care of generating the
-- required definitions of closures and serialisation dictionaries.
liftP :: Name -> Q Exp
liftP n = [| mkBasic $(mkClosure n) $(functionTDict n) |]
{-# INLINABLE liftP #-}
