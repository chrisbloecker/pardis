{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE KindSignatures #-}

module Main where

import Control.Concurrent.MVar
import Crawler
import Data.Binary
import Data.Typeable
import Pardis.Distributed
import System.Environment
import qualified Control.Distributed.Process.Node as N
import qualified Data.Map                         as M
import qualified Network.Transport.TCP            as TCP

-- Deriving the Binary instance for serialisation of Index.
deriving instance Binary Index

-- We have to make the functions that we want to use as processes monomorph.
-- We do that by giving them a new name (because we need it to refer to for
-- Template Haskell) and mentioning their type signature explicitly.
check   = length `is` 1   :: [String] -> Bool
head_   = head            :: URLs -> URL
bisectP = return . bisect :: URLs -> Process (URLs, URLs)
merge_  = uncurry merge   :: (Index, Index) -> Index
fst_    = fst             :: (URLs, URLs) -> URLs
snd_    = snd             :: (URLs, URLs) -> URLs
pageP   = page            :: URL -> Process Index

-- We use mkProcs to generate Cloud Haskell processes from the mentioned function.
-- remotable is used to generate the necessary closures and serialisation dictionaries.
mkProcs   [ 'continue,  'todo,  'check,  'head_,  'bisectP, 'merge_,  'fst_,  'snd_ ]
remotable [ 'continueP, 'todoP, 'checkP, 'head_P, 'bisectP, 'merge_P, 'fst_P, 'snd_P, 'pageP ]

-- | Crawls web pages and creates an index from the words that are found.
-- crawl uses continue to check whether there are URLs left to crawl.
-- If there are links left, then crawlAll is used to build up a binary tree
-- of processes that do the crawling. The leaf nodes of the tree hold
-- crawlOne processes while the inner nodes hold crawlMany processes.
-- Each crawlOne process builds a single-page index. These single-page indices
-- are then combined into an overall index by the crawlMany processes using
-- merge.
crawl :: Proc (Basic Process) Index Index
crawl = Repetition $(liftP 'continueP) ($(liftP 'todoP) `Sequence` crawlAll)
  where
    crawlAll  = Choice $(liftP 'checkP) crawlOne crawlMany
    --crawlOne  = $(liftP 'head_P) `Sequence` $(liftP 'pageP)
    crawlOne  = $(liftP 'head_P)  `Sequence` $(liftP 'pageP)
    crawlMany = $(liftP 'bisectP) `Sequence` Parallel $(liftP 'merge_P)
                                          ($(liftP 'fst_P) `Sequence` crawlAll)
                                          ($(liftP 'snd_P) `Sequence` crawlAll)

-- | The main program expects some command line arguments and builds up an index.
main :: IO ()
main = do
  args <- getArgs

  -- create a tcp socket and use it to run a Cloud Haskell node
  Right transport <- TCP.createTransport "localhost" "50000" TCP.defaultTCPParameters
  node            <- N.newLocalNode transport (__remoteTable N.initRemoteTable)

  -- an MVar for collecting the result of running processes
  mvar <- newEmptyMVar

  -- For now, we only use the local node to run processes on.
  let env = Env { getNode = return (N.localNodeId node) }

  case args of
    -- build an index starting from url and display the number of words that
    -- have been found
    [url] -> do
      N.runProcess node $ runProcess env crawl (emptyIndex { todo = [url] }) >>= liftIO . putMVar mvar
      Index{..} <- takeMVar mvar
      putStrLn $ "Indexed " ++ show (M.size index) ++ " words."

    -- build an index starting from url and look up the word given as query
    [url, query] -> do
      N.runProcess node $ runProcess env crawl (emptyIndex { todo = [url] }) >>= liftIO . putMVar mvar
      Index{..} <- takeMVar mvar
      putStrLn $ query ++ " was found under: " ++ show (M.lookup query index)

    _ -> putStrLn "Unexpected arguments"
