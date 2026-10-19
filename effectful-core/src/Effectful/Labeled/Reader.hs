{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Convenience functions for the 'Labeled' 'Reader' effect.
--
-- @since 2.4.0.0
module Effectful.Labeled.Reader
  ( -- * Effect
    Reader(..)

    -- ** Handlers
  , runReader
  , runReaderBy

    -- ** Operations
  , ask
  , askBy
  , asks
  , asksBy
  , local
  , localBy

    -- * Re-exports
  , Labeled(..)
  , Field(..)
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Labeled
import Effectful.Reader.Dynamic (Reader(..))
import Effectful.Reader.Dynamic qualified as R
import Polytropos.Core (Field(..))

-- | Run the 'Reader' effect with the given initial environment (via
-- "Effectful.Reader.Static").
runReader
  :: forall label r es a
   . HasCallStack
  => r
  -- ^ The initial environment.
  -> Eff (Labeled label (Reader r) : es) a
  -> Eff es a
runReader = runLabeled @label . R.runReader

-- | Run the 'Reader' effect using an OverloadedLabels field symbol (e.g. @#env@).
runReaderBy
  :: forall label r es a
   . HasCallStack
  => Field label
  -> r
  -> Eff (Labeled label (Reader r) : es) a
  -> Eff es a
runReaderBy _ = runReader @label

----------------------------------------
-- Operations

-- | Fetch the value of the environment.
ask
  :: forall label r es
  . (HasCallStack, Labeled label (Reader r) :> es)
  => Eff es r
ask = send $ Labeled @label Ask

-- | Fetch the value of the environment using an OverloadedLabels field symbol.
--
-- Example:
--
-- >>> askBy #config
askBy
  :: forall label r es
   . (HasCallStack, Labeled label (Reader r) :> es)
  => Field label
  -> Eff es r
askBy _ = ask @label

-- | Retrieve a function of the current environment.
--
-- @'asks' f ≡ f '<$>' 'ask'@
asks
  :: forall label r es a
   . (HasCallStack, Labeled label (Reader r) :> es)
  => (r -> a)
  -- ^ The function to apply to the environment.
  -> Eff es a
asks f = f <$> ask @label

-- | Retrieve a function of the current environment using an OverloadedLabels field symbol.
asksBy
  :: forall label r es a
   . (HasCallStack, Labeled label (Reader r) :> es)
  => Field label
  -> (r -> a)
  -> Eff es a
asksBy _ f = asks @label f

-- | Execute a computation in a modified environment.
--
-- @'runReader' r ('local' f m) ≡ 'runReader' (f r) m@
--
local
  :: forall label r es a
   . (HasCallStack, Labeled label (Reader r) :> es)
  => (r -> r)
  -- ^ The function to modify the environment.
  -> Eff es a
  -> Eff es a
local f = send . Labeled @label . Local f

-- | Execute a computation in a modified environment using an OverloadedLabels field symbol.
localBy
  :: forall label r es a
   . (HasCallStack, Labeled label (Reader r) :> es)
  => Field label
  -> (r -> r)
  -> Eff es a
  -> Eff es a
localBy _ = local @label
