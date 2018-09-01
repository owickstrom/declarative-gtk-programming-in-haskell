{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Declarative.Attributes where

import           GI.Gtk                                   ( Button(..)
                                                          , ScrolledWindow (..)
                                                          , ListBox (..)
                                                          , ListBoxRow (..)
                                                          , SelectionMode(..)
                                                          , PolicyType(..)
                                                          )
import           GI.Gtk.Declarative

type Event = ()

widgetAttributes :: Widget Event
widgetAttributes =
-- start snippet attributes-widget
  widget Button [#label := "Click Here"]
-- end snippet attributes-widget

binAttributes :: Widget Event
binAttributes =
-- start snippet attributes-bin
  bin ScrolledWindow [ #hscrollbarPolicy := PolicyTypeAutomatic ] $
    someSuperWideWidget
-- end snippet attributes-bin
  where
    someSuperWideWidget = widget Button []

containerAttributes :: Widget Event
containerAttributes =
-- start snippet attributes-container
  container ListBox [ #selectionMode := SelectionModeMultiple ] $
    children
-- end snippet attributes-container
  where
    children = bin ListBoxRow [] $ widget Button []
