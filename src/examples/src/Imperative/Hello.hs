{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.GI.Base
import qualified GI.Gtk       as Gtk

-- start snippet main
main :: IO ()
main = do
  _ <- Gtk.init Nothing
  win <- new Gtk.Window [#title := "Hi there"]
  #resize win 200 150
  _ <- on win #destroy Gtk.mainQuit
  button <- new Gtk.Button [#label := "Click me"]
  _ <- on button
     #clicked
     (set button [#sensitive := False, #label := "Thanks for clicking me"])
  #add win button
  #showAll win
  Gtk.main
-- end snippet main
