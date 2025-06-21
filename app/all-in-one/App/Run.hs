{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.Run (run) where

import App.Import
import qualified Hpack as H

-- >>> H.getOptions "." []
--
run :: RIO App ()
run = do
  logInfo "We're inside the application!"
  opt <- asks appOptions
  hpackOpt <- liftIO $ H.getOptions "." []
  case hpackOpt of
    Just (v, o) -> logInfo ("hpack verbose" <> displayShow v)
    Nothing -> pure ()
  pure ()
