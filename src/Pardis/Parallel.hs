{-# LANGUAGE TypeFamilies #-}

module Pardis.Parallel
  ( module Pardis.Parallel
  ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.DeepSeq         as Pardis.Parallel
import Data.Constraint
import Pardis                  as Pardis.Parallel

-- | Parallel implementation of Pardis using the IO monad.
--
-- Basic contain an IO action of type a -> IO b and a value of type
-- Dict (NFData b) that serves at witness for that b has an instance of NFData.
--
-- Execution environments are empty as there is no additional information needed
-- to execute IO actions.
--
-- Basic processes are executed by applying their intrinsic function to their
-- input.
--
-- Parallel processes are executed using a lightweight parallel thread started
-- with forkIO. The results of the sub-processes are fully evaluated to normal
-- form using force and the NFData constraint.
instance Pardis IO where
  data Basic IO a b = Basic (a -> IO b) (Dict (NFData b))
  data Env   IO     = Env

  runBasic _ (Basic f _) x = f x

  runParallel env c p q x = case (getDict p, getDict q) of
    (Dict, Dict) -> do
      mvar <- newEmptyMVar
      forkIO $ runProcess env p x >>= putMVar mvar . force
      qx   <- force <$> runProcess env q x
      px   <- takeMVar mvar
      runProcess env c (px, qx)

-- | Helper function to retrieve instance dictionaries from the base layer of
-- process composition.
getDict :: Proc (Basic IO) a b -> Dict (NFData b)
getDict (Lifted     p  ) = case p of Basic _ dict -> dict
getDict (Sequence   _ q) = getDict q
getDict (Repetition _ p) = getDict p
getDict (Choice   _ p _) = getDict p
getDict (Parallel c _ _) = getDict c

-- | Helper function to lift IO actions into processes. Through the NFData
-- constaint, instance dictionaries can be automatically created.
mkBasic :: NFData b => (a -> IO b) -> Proc (Basic IO) a b
mkBasic = Lifted . flip Basic Dict

-- | Helper function to lift pure functions into processes.
liftP :: NFData b => (a -> b) -> Proc (Basic IO) a b
liftP = mkBasic . fmap return
