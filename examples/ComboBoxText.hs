{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ComboBoxText (main) where

import           Control.Monad                  (void)
import           Data.Typeable                  (Typeable)
import           Data.Text (Text)
import qualified Data.Text                      as Text
import           Data.Vector                    (Vector)
import qualified Data.Vector as Vector

import qualified GI.Gtk                         as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import           GI.Gtk.Declarative.CustomWidget.ComboBoxText

-- * Example application using ComboBoxText
------------------------------------------------

data State = State
  { currentValue :: Double
  , choices :: Vector (Text, Int)
  , lastSelection :: Maybe Int
  , choicesList :: [Text]
  }

data Event = Closed | Next | Prev | SetChoice Int

view' :: State -> AppView Gtk.Window Event
view' (State{..}) =
  bin
      Gtk.Window
      [ #title := "Hello"
      , on #deleteEvent (const (True, Closed))
      , #widthRequest := 400
      , #heightRequest := 300
      ]
    $ centered $ container Gtk.Box []
      [ widget Gtk.Label [#label := Text.pack (show currentValue)]
      , widget Gtk.Button [#label := "Prev", on #clicked Prev]
      , fmap (SetChoice . position) $
          BoxChild defaultBoxChildProperties $ comboBoxText $
            ComboBoxTextLines choicesList lastSelection
      , widget Gtk.Button [#label := "Next", on #clicked Next]
      ]

-- Helper that vertically and horizontally centers a widget
centered :: Typeable e => Widget e -> Widget e
centered w = container
  Gtk.Box
  [#orientation := Gtk.OrientationVertical]
  [ BoxChild defaultBoxChildProperties { expand = True, padding = 10 }
      $ container
          Gtk.Box
          [#orientation := Gtk.OrientationHorizontal]
          [BoxChild defaultBoxChildProperties { expand = True, padding = 10 } w]
  ]

update' :: State -> Event -> Transition State Event
update' _ Closed        = Exit
update' s@State{..} (SetChoice p) = Transition s
  { currentValue = fromIntegral . snd $ choices Vector.! p
  , lastSelection = Just p
  } $ return Nothing
-- Delegate the next two to SetChoice
update' s@State{..} Next
  | Just p <- lastSelection, p + 1 < Vector.length choices =
      Transition s . return . Just $ SetChoice (p+1)
  | otherwise = Transition s $ return Nothing
update' s@State{..} Prev
  | Just p <- lastSelection, p > 0 =
      Transition s . return . Just $ SetChoice (p-1)
  | otherwise = Transition s $ return Nothing

main :: IO ()
main = void $ run App
  { view         = view'
  , update       = update'
  , inputs       = []
  , initialState = State
    { currentValue = 1.0
    , choices = choices
    , choicesList = map fst $ Vector.toList choices
    , lastSelection = Nothing
    }
  }
  where
  choices = [("One", 1), ("Two", 2), ("Three", 3)]
