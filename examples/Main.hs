{-# LANGUAGE LambdaCase #-}
module Main where

import           System.Environment
import           System.IO

import qualified AddBoxes
import qualified CSS
import qualified ComboBoxText
import qualified CustomWidget
import qualified Dialog
import qualified Exit
import qualified FileChooserButton
import qualified Functor
import qualified Hello
import qualified ListBox
import qualified ManyBoxes
import qualified MenuBar
import qualified Paned
import qualified Todo

main :: IO ()
main =
  let examples = [ ("AddBoxes", AddBoxes.main)
                 , ("CustomWidget", CustomWidget.main)
                 , ("ComboBoxText", ComboBoxText.main)
                 , ("FileChooserButton", FileChooserButton.main)
                 , ("Hello", Hello.main)
                 , ("ListBox", ListBox.main)
                 , ("Functor", Functor.main)
                 , ("Exit", Exit.main)
                 , ("ManyBoxes", ManyBoxes.main)
                 , ("MenuBar", MenuBar.main)
                 , ("CSS", CSS.main)
                 , ("Paned", Paned.main)
                 , ("Dialog", Dialog.main)
                 , ("Todo", Todo.main)
                 ]
  in getArgs >>= \case
    [example] ->
      case lookup example examples of
        Just main' -> main'
        Nothing -> hPutStrLn stderr ("No example available with name: " <> example)
    _ -> hPutStrLn stderr ("Usage: gi-gtk-declarative-example NAME\n\nWhere NAME is any of:\n" <> unlines (map (("  " <>) . fst) examples))
