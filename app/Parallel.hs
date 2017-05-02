{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Crawler
import Pardis.Parallel
import System.Environment
import qualified Data.Map as M

-- Deriving the NFData instance for full evaluation of Index.
deriving instance NFData Index

-- | Crawls web pages and creates an index from the words that are found.
-- crawl uses continue to check whether there are URLs left to crawl.
-- If there are links left, then crawlAll is used to build up a binary tree
-- of processes that do the crawling. The leaf nodes of the tree hold
-- crawlOne processes while the inner nodes hold crawlMany processes.
-- Each crawlOne process builds a single-page index. These single-page indices
-- are then combined into an overall index by the crawlMany processes using
-- merge.
crawl :: Proc (Basic IO) Index Index
crawl = Repetition (liftP continue) (liftP todo `Sequence` crawlAll)
  where
    crawlAll  = Choice (liftP $ length `is` 1) crawlOne crawlMany
    crawlOne  = liftP head `Sequence` mkBasic page
    crawlMany = liftP bisect `Sequence` Parallel (liftP $ uncurry merge)
                                          (liftP fst `Sequence` crawlAll)
                                          (liftP snd `Sequence` crawlAll)

-- | The main program expects some command line arguments and builds up an index.
main :: IO ()
main = do
  args <- getArgs
  case args of
    -- build an index starting from url and display the number of words that
    -- have been found
    [url] -> do
      Index{..} <- runProcess Env crawl (emptyIndex { todo = [url] })
      putStrLn $ "Indexed " ++ show (M.size index) ++ " words."

    -- build an index starting from url and look up the word given as query
    [url, query] -> do
      Index{..} <- runProcess Env crawl (emptyIndex { todo = [url] })
      putStrLn $ query ++ " was found under: " ++ show (M.lookup query index)

    _ -> putStrLn "Unexpected arguments"
