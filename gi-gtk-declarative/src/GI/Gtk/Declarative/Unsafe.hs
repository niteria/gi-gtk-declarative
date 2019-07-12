{-# LANGUAGE MagicHash #-}
module GI.Gtk.Declarative.Unsafe where

import           GHC.Exts (reallyUnsafePtrEquality#)

-- These functions are used to speed up comparisons.
--
-- For some widgets their options can be expensive to compare, but change really
-- infrequently or are constant. An example would be a ComboBoxText with
-- hundreds of choices. In that case it is beneficial to test for pointer
-- equality to optimistically stop equality checks if the exact same object
-- is seen again.
--
-- This implementation is based on (stolen) the code from the reflex library:
-- http://hackage.haskell.org/package/reflex-0.6/docs/src/Reflex.Dynamic.Uniq.html#unsafePtrEq
--
-- There are some old claims (2010) that reallyUnsafePtrEquality# can produce
-- false positives:
-- https://mail.haskell.org/pipermail/haskell-cafe/2010-June/079532.html
--
-- This (2017) suggests that the below implementation is safe:
-- https://mail.haskell.org/pipermail/haskell-cafe/2017-November/128218.html


-- Can and will often give false negatives, meaning that if it returns
-- False the objects can still be equal.
unsafePtrEq :: a -> a -> Bool
unsafePtrEq a b = case a `seq` b `seq` reallyUnsafePtrEquality# a b of
  0# -> False
  _ -> True

-- Can and will often give false positives, meaning that if it returns
-- True the objects can still be equal.
unsafePtrNeq :: a -> a -> Bool
unsafePtrNeq a b = case a `seq` b `seq` reallyUnsafePtrEquality# a b of
  0# -> True
  _ -> False
