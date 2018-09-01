{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           GI.Gdk.Structs.RGBA
import           GI.Gtk                        (ColorButton (..), getColorButtonRgba)
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

-- start snippet model-event
type Model = RGBA

data Event = ColorChanged (Maybe RGBA)
-- end snippet model-event

colorButton :: Model -> Widget Event
-- start snippet onM
colorButton color =
  widget
  ColorButton
  [ #title := "Selected color"
  , #rgba := color
  , onM #colorSet (fmap ColorChanged . getColorButtonRgba)
  ]
-- end snippet onM

-- start snippet update
update' :: Model -> Event -> (Model, IO (Maybe Event))
update' _ (ColorChanged (Just newColor)) =
  (newColor, rGBAToString newColor >>= print >> return Nothing)
update' oldColor (ColorChanged Nothing) =
  (oldColor, return Nothing)
-- end snippet update

-- start snippet main
main :: IO ()
main = do
  color <- newZeroRGBA
  run "Pick a Color" (Just (100, 75)) app color
  where app = App {view = colorButton, update = update', inputs = []}
-- end snippet main
