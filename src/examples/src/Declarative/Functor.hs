{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Data.Functor                  (($>))
import           Data.Text                     (Text)
import qualified Data.Text                     as Text

import           GI.Gtk                        (Box (..), Button (..),
                                                Label (..), Orientation (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

-- start snippet button
data ButtonEvent = ButtonClicked

clickyButton :: Text -> Widget ButtonEvent
-- end snippet button
clickyButton label =
  widget Button [#label := label, on #clicked ButtonClicked]

data Model = Model { count :: Integer }

-- start snippet fmap
data MyEvent = Incr | Decr

incrDecrButtons :: Widget MyEvent
incrDecrButtons =
  container Box [#orientation := OrientationHorizontal] $ do
    boxChild True True 0 $ clickyButton "-1" $> Decr
    boxChild True True 0 $ clickyButton "+1" $> Incr
-- end snippet fmap


incrDecrView :: Model -> Widget MyEvent
incrDecrView Model{..} =
  container Box [#orientation := OrientationVertical] $ do
    boxChild True True 0 $
      widget Label [ #label := Text.pack (show count) ]
    boxChild True True 0 incrDecrButtons

update' :: Model -> MyEvent -> (Model, IO (Maybe MyEvent))
update' Model {..} Incr = (Model (count + 1), return Nothing)
update' Model {..} Decr = (Model (count - 1), return Nothing)

main :: IO ()
main = run "Functor!" (Just (639, 480)) app (Model 0)
  where app = App {view = incrDecrView, update = update', inputs = []}
