{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.Run (run) where

import App.Import

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
