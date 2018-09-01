{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Data.GI.Base
import           Data.IORef
import           Data.Text                                ( Text )
import qualified GI.Gtk                        as Gtk

-- start snippet editableNamesList-signature
editableNamesList :: [Text] -> IO (Gtk.Widget, Chan [Text])
-- end snippet editableNamesList-signature
-- start snippet editableNamesList
editableNamesList initialNames = do
  updates  <- newChan
  namesRef <- newIORef initialNames
  list     <- new Gtk.ListBox []

  forM_ (zip initialNames [0 ..]) $ \(name, i) -> do
    textEntry <- new Gtk.Entry [#text := name]
    void . on textEntry #changed $ do
      newName <- get textEntry #text
      writeChan updates =<<
        (atomicModifyIORef' namesRef $ \oldNames ->
          let newNames = oldNames & ix i .~ newName
          in (newNames, newNames))
    #add list textEntry

  widget <- Gtk.toWidget list
  return (widget, updates)
-- end snippet editableNamesList


main :: IO ()
main = do
  _   <- Gtk.init Nothing
  win <- new Gtk.Window [#title := "Editable Names"]
  #resize win 640 480
  _               <- on win #destroy Gtk.mainQuit

-- start snippet main
  (list, updates) <- editableNamesList ["Alice", "Bob", "Carol"]
  void . forkIO . forever $ do
    names <- readChan updates
    print names
  #add win list
-- end snippet main

  #showAll win
  Gtk.main
