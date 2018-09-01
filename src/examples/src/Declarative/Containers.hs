{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Declarative.Containers where

import           GI.Gtk                                   ( Button(..)
                                                          , Box(..)
                                                          , ListBox(..)
                                                          , ListBoxRow(..)
                                                          , ScrolledWindow(..)
                                                          )
import           GI.Gtk.Declarative

data Event = MyEvent

containerBox :: Widget Event
containerBox =
-- start snippet box
  container Box [] $ do
    boxChild False False 0 $ widget Button []
    boxChild True True 0 $ widget Button []
-- end snippet box

containerListBox :: Widget Event
containerListBox =
-- start snippet list-box
  container ListBox [] $ do
    bin ListBoxRow [] $ widget Button []
    bin ListBoxRow [] $ widget Button []
-- end snippet list-box

containerScrolledWindow :: Widget Event
containerScrolledWindow =
-- start snippet scrolled-window
  bin ScrolledWindow [] $
    widget Button []
-- end snippet scrolled-window
