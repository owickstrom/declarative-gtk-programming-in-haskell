{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Lens
import           Control.Monad
import           Data.GI.Base
import           Data.IORef
import           Data.Text                                ( Text )
import qualified GI.Gtk                        as Gtk

-- start snippet editableNamesList-signature
editableNamesList :: IORef [Text] -> IO Gtk.Widget
-- end snippet editableNamesList-signature
-- start snippet editableNamesList
editableNamesList namesRef = do
  list     <- new Gtk.ListBox []
  initialNames <- readIORef namesRef

  forM_ (zip initialNames [0 ..]) $ \(name, i) -> do
    textEntry <- new Gtk.Entry [#text := name]
    void . on textEntry #changed $ do
      newName <- get textEntry #text
      atomicModifyIORef' namesRef $ \oldNames ->
        let newNames = oldNames & ix i .~ newName
        in (newNames, ())
    #add list textEntry

  Gtk.toWidget list
-- end snippet editableNamesList


main :: IO ()
main = do
  _   <- Gtk.init Nothing
  win <- new Gtk.Window [#title := "Editable Names"]
  #resize win 640 480
  _               <- on win #destroy Gtk.mainQuit

-- start snippet main
  namesRef <- newIORef ["Alice", "Bob", "Carol"]
  list <- editableNamesList namesRef
  #add win list
-- end snippet main

  #showAll win
  Gtk.main
