{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           GI.Gtk                        (Button (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import qualified Data.Text as Text

-- start snippet model-event
type Model = Int

data Event = ButtonClicked
-- end snippet model-event

counterButton :: Model -> Widget Event
-- start snippet on
counterButton clickCount =
  let msg = "I've been clicked "
            <> Text.pack (show clickCount)
            <> " times."
  in widget
      Button
      [ #label := msg
      , on #clicked ButtonClicked
      ]
-- end snippet on

-- start snippet update
update' :: Model -> Event -> (Model, IO (Maybe Event))
update' n ButtonClicked =
  (succ n, return Nothing)
-- end snippet update

-- start snippet main
main :: IO ()
main =
  run "Button Clicks" (Just (100, 75)) app 0
  where app = App {view = counterButton, update = update', inputs = []}
-- end snippet main
