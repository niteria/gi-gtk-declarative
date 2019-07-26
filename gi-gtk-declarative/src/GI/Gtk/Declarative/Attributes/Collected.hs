{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

-- | Internal helpers for applying attributes and signal handlers to GTK+
-- widgets.
module GI.Gtk.Declarative.Attributes.Collected
  ( CollectedProperties
  , Collected(..)
  , collectAttributes
  , constructProperties
  , updateProperties
  , updatePropertiesSingleWidget
  , updateClasses
  )
where

import           Data.Foldable
import qualified Data.Text                     as Text
import           Data.Text                                ( Text )
import qualified Data.HashSet                  as HashSet
import qualified Data.HashMap.Strict           as HashMap
import           Data.HashMap.Strict                      ( HashMap )
import qualified Data.GI.Base.Attributes       as GI
import qualified GI.Gtk                        as Gtk
import           Data.Typeable
import           GHC.TypeLits
import           Data.Vector                              ( Vector )

import           GI.Gtk.Declarative.Attributes
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Debug.Trace (trace)

-- | A collected property key/value pair, to be used when
-- settings properties when patching widgets.
data CollectedProperty widget where
  CollectedProperty
    :: (GI.AttrOpAllowed 'GI.AttrConstruct info widget
      , GI.AttrOpAllowed 'GI.AttrSet info widget
      , GI.AttrGetC info widget attr getValue
      , GI.AttrSetTypeConstraint info setValue
      , KnownSymbol attr
      , Typeable attr
      , Eq setValue
      , Typeable setValue
      )
   => GI.AttrLabelProxy attr -> setValue -> CollectedProperty widget

-- | A collected map of key/value pairs, where the type-level property
-- names are represented as 'Text' values. This is used to calculate
-- differences in old and new property sets when patching.
type CollectedProperties widget = HashMap Text (CollectedProperty widget)

-- | All the collected properties and classes for a widget. These are based
-- on the 'Attribute' list in the declarative markup, but collected separately
-- into more efficient data structures, optimized for patching.
data Collected widget event = Collected
  { collectedClasses :: ClassSet
  , collectedProperties :: CollectedProperties widget
  }

instance Semigroup (Collected widget event) where
  c1 <> c2 =
    Collected
      (collectedClasses c1 <> collectedClasses c2)
      (collectedProperties c1 <> collectedProperties c2)

instance Monoid (Collected widget event) where
  mempty = Collected mempty mempty

-- | Collect declarative markup attributes to the patching-optimized
-- 'Collected' data structure.
collectAttributes :: Vector (Attribute widget event) -> Collected widget event
collectAttributes = foldl' go mempty
 where
  go
    :: Collected widget event
    -> Attribute widget event
    -> Collected widget event
  go Collected {..} = \case
    attr := value -> Collected
      { collectedProperties = HashMap.insert (Text.pack (symbolVal attr))
                                             (CollectedProperty attr value)
                                             collectedProperties
      , ..
      }
    Classes classSet ->
      Collected {collectedClasses = collectedClasses <> classSet, ..}
    _ -> Collected {..}

-- | Create a list of GTK construct operations based on collected
-- properties, used when creating new widgets.
constructProperties
  :: Collected widget event -> [GI.AttrOp widget 'GI.AttrConstruct]
constructProperties c = map
  (\(CollectedProperty attr value) -> attr Gtk.:= value)
  (HashMap.elems (collectedProperties c))

-- | Update the changed properties of a widget, based on the old and new
-- collected properties.
updateProperties
  :: widget -> CollectedProperties widget -> CollectedProperties widget -> IO ()
updateProperties (widget' :: widget) oldProps newProps = do
  let toAdd  = HashMap.elems (HashMap.difference newProps oldProps)
      setOps = mconcat
        (HashMap.elems (HashMap.intersectionWith toMaybeSetOp oldProps newProps)
        )
  let props = (map (toSetOp (Proxy @widget)) toAdd <> setOps)
  GI.set widget' props
 where
  toSetOp
    :: Proxy widget
    -> CollectedProperty widget
    -> Gtk.AttrOp widget 'GI.AttrSet
  toSetOp _ (CollectedProperty attr value) = attr Gtk.:= value

  toMaybeSetOp
    :: CollectedProperty widget
    -> CollectedProperty widget
    -> [Gtk.AttrOp widget 'GI.AttrSet]
  toMaybeSetOp (CollectedProperty attr (v1 :: t1)) (CollectedProperty _ (v2 :: t2))
    = case eqT @t1 @t2 of
      Just Refl | v1 /= v2 -> pure (attr Gtk.:= v2)
      _                    -> mempty

updatePropertiesSingleWidget
  :: Typeable widget => widget -> CollectedProperties widget -> CollectedProperties widget -> IO ()
updatePropertiesSingleWidget (widget' :: widget) oldProps newProps = do
  let toAdd  = HashMap.elems (HashMap.difference newProps oldProps)
      toAddKeys  = HashMap.keys (HashMap.difference newProps oldProps)
      setOps = mconcat
        (HashMap.elems setOpsHM
        )
      setOpsHM = (HashMap.intersectionWith toMaybeSetOp oldProps newProps)
      setOpsKeys = mconcat
        (HashMap.keys setOpsHM
        )
  putStrLn ("updateProperties toAdd: " ++ show toAddKeys ++ " setOps: " ++ show setOpsKeys)
  -- threadDelay 1000000
  let props = (map (toSetOp (Proxy @widget)) toAdd <> setOps)
  mapM_ getOp setOps
  -- when (not $ null props) $ do
  GI.set widget' props
  -- putStrLn ("after updateProperties toAdd: " ++ show toAddKeys ++ " setOps: " ++ show setOpsKeys)
  -- threadDelay 1000000
 where
  getOp :: Gtk.AttrOp widget 'GI.AttrSet -> IO ()
  getOp (k Gtk.:= (v :: t)) =
    case eqT @widget @Gtk.Entry of
      Just Refl -> do
        putStrLn "Found entry"
        t <- Gtk.entryGetText widget'
        putStrLn ("getOp " ++ show t)

      _ -> putStrLn "Got something else"
  getOp _ = return ()
  toSetOp
    :: Proxy widget
    -> CollectedProperty widget
    -> Gtk.AttrOp widget 'GI.AttrSet
  toSetOp _ (CollectedProperty attr value) = attr Gtk.:= value

  toMaybeSetOp
    :: CollectedProperty widget
    -> CollectedProperty widget
    -> [Gtk.AttrOp widget 'GI.AttrSet]
  toMaybeSetOp (CollectedProperty attr (v1 :: t1)) (CollectedProperty _ (v2 :: t2))
    = case eqT @t1 @t2 of
      Just Refl | v1 /= v2 ->
        case eqT @t1 @Text of
          Just Refl -> trace ("toMaybeSetOp: " ++ show (v1, v2)) $ pure (attr Gtk.:= v2)
          _ -> pure (attr Gtk.:= v2)
      _                    -> mempty

-- | Update the style context's classes to only include the new set of
-- classes (last argument).
updateClasses :: Gtk.StyleContext -> ClassSet -> ClassSet -> IO ()
updateClasses sc old new = do
  let toAdd    = HashSet.difference new old
      toRemove = HashSet.difference old new
  -- putStrLn ("updateClasses toAdd: " ++ show toAdd ++ " toRemove: " ++ show toRemove)
  -- threadDelay 1000000
  mapM_ (Gtk.styleContextAddClass sc)    toAdd
  mapM_ (Gtk.styleContextRemoveClass sc) toRemove
  -- putStrLn ("after updateClasses toAdd: " ++ show toAdd ++ " toRemove: " ++ show toRemove)
  -- threadDelay 1000000
