{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Declarative.Widgets where

import           GI.Gtk                                   ( Button(..)
                                                          , CheckButton(..)
                                                          , Label(..)
                                                          )
import           GI.Gtk.Declarative

type Event = ()

widgetNoAttrs :: Widget Event
widgetNoAttrs =
-- start snippet widget-no-attrs
  widget Button []
-- end snippet widget-no-attrs

widgetNoAttrs2 :: Widget Event
widgetNoAttrs2 =
-- start snippet widget-no-attrs-2
  widget CheckButton []
-- end snippet widget-no-attrs-2

widgetOneAttr :: Widget Event
widgetOneAttr =
-- start snippet widget-one-attr
  widget Label [#label := "Hello, I'm a label."]
-- end snippet widget-one-attr

widgetClasses :: Widget Event
widgetClasses =
-- start snippet widget-classes
  widget Button [classes ["big-button"], #label := "CLICK ME"]
-- end snippet widget-classes
