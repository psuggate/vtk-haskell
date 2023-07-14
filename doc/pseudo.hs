{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeOperators #-}

module Pseudo where

import           Control.Arrow
import           Relude

import           Data.VTK.Parser
import           Data.VTK.Raw


withVtkFile
  :: forall ts m r. MonadIO m
  => VtkData ts
  => FilePath
  -> (VTK ts -> m r)
  -> m r
withVtkFile fp action = do
  raw <- liftIO $ readRawVtk fp
  parseVtk (Proxy :: Proxy ts) raw

withVtkFileA
  :: forall ts r arr. ArrowApply arr
  => VtkData ts
  => (VTK ts `arr` r)
  -> FilePath `arr` r
withVtkFileA fun = readRawVtkA >>> parseVtkA >>> fun
