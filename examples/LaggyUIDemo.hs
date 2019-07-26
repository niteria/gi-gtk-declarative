module LaggyUIDemo where

import Control.Monad (void)
import Control.Concurrent
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import qualified GI.GLib as GLib

-- Demo of what happens when your render loop is too slow
-- Type really fast and observe the cursor drifting to the beginning

main :: IO ()
main = do
  void $ Gtk.init Nothing

  win <- Gtk.windowNew Gtk.WindowTypeToplevel
  void $ Gtk.onWidgetDestroy win Gtk.mainQuit

  box <- Gtk.boxNew Gtk.OrientationVertical 2
  entry <- Gtk.entryNew
  chan <- newChan

  void $ Gtk.onEditableChanged entry $ do
    str <- Gtk.entryGetText entry
    writeChan chan str

  void $ forkOS $ sequence_ $ repeat $ do
    str <- readChan chan
    threadDelay 100000 -- some arbitrary delay, the equivalent of updating the
    -- state and generating a new view
    Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
      Gtk.entrySetText entry str
      return False

  Gtk.containerAdd box entry
  Gtk.containerAdd win box

  Gtk.widgetShowAll win
  Gtk.main
