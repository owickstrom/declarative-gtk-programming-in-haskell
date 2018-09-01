{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens                  (ix, (.~))
import           Control.Monad                 (zipWithM_)
import           Data.Function                 ((&))
import           Data.Functor                  (($>))
import           Data.Text                     (Text)
import           GI.Gtk                        (Entry (..), ListBox (..),
                                                ListBoxRow (..), entryGetText)
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

-- start snippet model-event
data Model = Model [Text]

data Event = NameChanged Int Text
-- end snippet model-event

-- start snippet view
view' :: Model -> Widget Event
view' (Model names) =
  container ListBox [] (zipWithM_ renderNameEntry names [0 ..])
  where
    renderNameEntry name i =
      bin ListBoxRow [] $
        widget Entry [ #text := name
                     , onM #changed (fmap (NameChanged i) . entryGetText)
                     ]
-- end snippet view

-- start snippet update
update' :: Model -> Event -> (Model, IO (Maybe Event))
update' (Model names) (NameChanged i newName) =
  ( Model (names & ix i .~ newName)
  , print newName $> Nothing
  )
-- end snippet update

-- start snippet main
main :: IO ()
main = run "Editable Names"
           (Just (640, 480))
           app
           (Model ["Alice", "Bob", "Carol"])
  where app = App {view = view', update = update', inputs = []}
-- end snippet main
