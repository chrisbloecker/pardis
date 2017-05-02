{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{- A rudimentary web crawler that serves as an application example for the paper
/Pardis: A Process Calculus for Parallel and Distributed Programming in Haskell/
by Christopher Blöcker and Ulrich Hoffmann.
-}
module Crawler where

import Control.Exception      (handle)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char              (isAlpha)
import Data.List              ((\\), union, isPrefixOf, nub)
import Data.Map.Strict        (Map, empty, fromList, unionWith)
import GHC.Generics
import Network.HTTP.Conduit
import Text.HTML.TagSoup
import Text.StringLike        (StringLike, toString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.List                  as L

-- | A URL is simply a String
type URL  = String

-- | URLs is a list of String
type URLs = [URL]

-- | An index holds URLs that have been crawled already, URLs that should be
-- crawled and a Map from Strings to URLs, i.e., the index that has been created
-- by crawling.
data Index = Index { done :: URLs, todo :: URLs, index :: Map String URLs }
  deriving (Generic)

-- | Creates an empty index.
emptyIndex :: Index
emptyIndex = Index [] [] empty

-- | Merges two indices.
merge :: Index -> Index -> Index
merge i1 i2 = let done'  = done i1 `union` done i2
                  todo'  = todo i1 `union` todo i2 \\ done'
                  index' = unionWith union (index i1) (index i2)
              in Index done' todo' index'

-- | Convenience function to express predicates.
is :: Eq b => (a -> b) -> b -> a -> Bool
is f v a = f a == v

-- | Uses TagSoup to extract links from a list of tags. We take the values of
-- href attributes in a tags and use them as links. For now, we are only
-- interested in absolute links.
getLinks :: (StringLike str, Show str) => [Tag str] -> [String]
getLinks = filter (\link -> "http://" `isPrefixOf` link || "https://" `isPrefixOf` link)
         . map (toString . fromAttrib "href")
         . filter isLink
  where
    -- Checks whether a tag is a link.
    isLink t@(TagOpen _ ((attr,_):_)) = isTagOpenName "a" t && attr == "href"
    isLink _                          = False

-- | Extracts words from a list of tags. We use TagSoup to extract the inner
-- text of the given tags and retain all words that contain only of characters
-- and are at least of length 5.
getWords :: (StringLike str) => [Tag str] -> [String]
getWords = nub . filter (\w -> length w >= 5 && all isAlpha w) . L.words . toString . innerText

-- | Page retrieves the html document from a given URL and creates a single-page
-- index from its content. In case an exception occurs when retrieving the web
-- page, we return an empty index.
page :: MonadIO m => URL -> m Index
page url = liftIO $ handle (\(_ :: HttpException) -> return emptyIndex) $ do
  html <- parseTags . BS.unpack <$> simpleHttp url
  let (links, words) = (getLinks html, getWords html)
  return $ Index [url] links (fromList [(w, [url]) | w <- words])

-- | Checks whether the list under todo in an index is empty.
continue :: Index -> Bool
continue = not . null . todo

-- | Takes a list and splits it into into half. The lenghts of the resulting
-- lists differ by at most 1.
bisect :: [a] -> ([a], [a])
bisect l  = let n = length l `div` 2 in (take n l, drop n l)
