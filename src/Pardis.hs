{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
{- [Pardis]

This is the implementation of Pardis, as described in
/Pardis: A Process Calculus for Parallel and Distributed Programming in Haskell/
by Christopher Blöcker and Ulrich Hoffmann.

-}
module Pardis
  ( Proc (..)
  , Pardis (..)
  , runProcess
  ) where

-- | The data type of processes. Lifted is used to lift basic processes into the
-- calculus. The other data constructors correspond to our process combinators
-- and are used to build up composed processes.
data Proc p a b where
  Lifted     :: (Pardis m, p ~ Basic m) => p a b            -> Proc p a b
  Sequence   ::                    Proc p a c -> Proc p c b -> Proc p a b
  Repetition :: Proc p a Bool   -> Proc p a a               -> Proc p a a
  Choice     :: Proc p a Bool   -> Proc p a b -> Proc p a b -> Proc p a b
  Parallel   :: Proc p (c, d) b -> Proc p a c -> Proc p a d -> Proc p a b

-- | A type alias for processes for more concise code.
type P m a b = Proc (Basic m) a b

-- | The Pardis type class. Pardis associates a basic process type and
-- evaluation environments with a monad m. Pardis also defined evaluation
-- functions for basic and composed processes, and gives default implementations
-- for the cases Sequence, Repetition and Choice.
class Monad m => Pardis m where
  data Basic m :: * -> * -> *
  data Env   m :: *

  runBasic      :: Env m -> Basic m a b  ->                       a -> m b
  runSequence   :: Env m                 -> P m a c -> P m c b -> a -> m b
  runRepetition :: Env m -> P m a Bool   -> P m a a            -> a -> m a
  runChoice     :: Env m -> P m a Bool   -> P m a b -> P m a b -> a -> m b
  runParallel   :: Env m -> P m (c, d) b -> P m a c -> P m a d -> a -> m b

  runSequence env p q x = runProcess env p x >>= runProcess env q

  runRepetition env r p x = do
    b <- runProcess env r x
    if b then runProcess env (Sequence p (Repetition r p)) x
      else return x

  runChoice env r p q x = do
    b <- runProcess env r x
    runProcess env (if b then p else q) x

-- | A function to execute processes using evaluation environments.
runProcess :: Pardis m => Env m -> Proc (Basic m) a b -> a -> m b
runProcess env (Lifted       p  ) = runBasic      env   p
runProcess env (Sequence     p q) = runSequence   env   p q
runProcess env (Choice     r p q) = runChoice     env r p q
runProcess env (Parallel   r p q) = runParallel   env r p q
runProcess env (Repetition r p  ) = runRepetition env r p
{-# INLINABLE runProcess #-}
