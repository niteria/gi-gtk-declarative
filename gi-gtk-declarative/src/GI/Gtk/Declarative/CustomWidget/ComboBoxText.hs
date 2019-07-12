{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GI.Gtk.Declarative.CustomWidget.ComboBoxText
  ( ComboBoxTextLines(..)
  , ComboBoxTextEvent(..)
  , comboBoxText
  )where

import           Control.Monad                  (forM_)
import           Data.Text                      (Text)

import qualified GI.GObject                     as GI
import qualified GI.Gtk                         as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.EventSource (fromCancellation)
import           GI.Gtk.Declarative.Unsafe      (unsafePtrNeq)

data ComboBoxTextLines =
  ComboBoxTextLines
  { choices :: [Text]
  , mbPosition :: (Maybe Int)
  }
  deriving (Eq, Show)

data ComboBoxTextEvent =
  ComboBoxTextSelected
  { choice :: Text
  , position :: Int
  }

comboBoxText :: ComboBoxTextLines -> Widget ComboBoxTextEvent
comboBoxText customParams = Widget
  (CustomWidget
    { customWidget
    , customCreate
    , customPatch
    , customSubscribe
    , customParams
    }
  )
  where
  customWidget = Gtk.ComboBoxText
  customCreate ComboBoxTextLines{..} = do
    comboBox <- Gtk.new Gtk.ComboBoxText []
    forM_ choices $ \choice ->
      #append comboBox Nothing choice
    updatePos mbPosition comboBox
    return (comboBox, ())
  updatePos mbPosition (comboBox :: Gtk.ComboBoxText) = do
    case mbPosition of
      Just pos -> #setActive comboBox $ fromIntegral pos
      Nothing -> #setActive comboBox (-1)
  customPatch
    old@(ComboBoxTextLines oldChoices oldMbPosition)
    new@(ComboBoxTextLines newChoices newMbPosition)
    ()
    | not needToUpdateChoices && not needToUpdatePosition = CustomKeep
    | not needToUpdateChoices = CustomModify $ updatePos newMbPosition
    | otherwise = CustomModify $ \comboBox -> do
        #removeAll comboBox
        forM_ newChoices $ \choice ->
          #append comboBox Nothing choice
        updatePos newMbPosition comboBox
    where
    needToUpdateChoices = (old `unsafePtrNeq` new) &&
      (oldChoices `unsafePtrNeq` newChoices) && oldChoices /= newChoices
    needToUpdatePosition = needToUpdateChoices || oldMbPosition /= newMbPosition

  customSubscribe _ (comboBox :: Gtk.ComboBoxText) cb = do
    h <- Gtk.on comboBox #changed $ do
      pos <- fromIntegral <$> #getActive comboBox
      text <- #getActiveText comboBox
      cb $ ComboBoxTextSelected text pos
    return (fromCancellation (GI.signalHandlerDisconnect comboBox h))
