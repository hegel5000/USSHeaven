{-# OPTIONS_GHC -Wall #-}

module ActorContext where

import Control.Monad.Reader (ask, lift, runReaderT, ReaderT)

import Identifiers (ActorID)

type ActorCxt = ReaderT ActorID

--This is kinda just there for documentation.  I usually just use ask.
--Of course, the type signature is a bit more precise here, so thisUID 
--might make it easier to figure out how to make things compile.
thisUID :: (Monad m) => ActorCxt m ActorID
thisUID = ask

toActorCxt :: (Monad m) => (ActorID -> m a) -> ActorCxt m a
toActorCxt fromUID = lift . fromUID =<< ask

withActorCxt :: (Monad m) => ActorID -> ActorCxt m a -> m a
withActorCxt = flip runReaderT

withActorID :: (Monad m) => ActorCxt m a -> ActorID -> m a
withActorID = runReaderT
