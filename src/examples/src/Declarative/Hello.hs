{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           GI.Gtk                        (Button (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

-- start snippet model-event
data Model = NotClicked | Clicked

data Event = ButtonClicked
-- end snippet model-event

-- start snippet view
view' :: Model -> Widget Event
view' = \case
  NotClicked ->
    widget Button [#label := "Click me", on #clicked ButtonClicked]
  Clicked ->
    widget Button [#sensitive := False, #label := "Thanks for clicking me"]
-- end snippet view

-- start snippet update
update' :: Model -> Event -> (Model, IO (Maybe Event))
update' _ ButtonClicked = (Clicked, return Nothing)
-- end snippet update

-- start snippet main
main :: IO ()
main = run "Hi there" (Just (200, 150)) app NotClicked
  where app = App {view = view', update = update', inputs = []}
-- end snippet main
