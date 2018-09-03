{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Declarative.Containers where

import           GI.Gtk                                   ( Button(..)
                                                          , CheckButton(..)
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
    boxChild True True 0 $ widget CheckButton []
-- end snippet box

containerListBox :: Widget Event
containerListBox =
-- start snippet list-box
  container ListBox [] $ do
    bin ListBoxRow [] $ widget Button []
    bin ListBoxRow [] $ widget CheckButton []
-- end snippet list-box
