{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Effectful.Soup
-- Description : Composable multi-context Reader soup effect backed by Polytropos
-- Copyright   : (c) 2026, Nadia Yvette Chambers
-- License     : BSD-3-Clause
-- Maintainer  : nadia.yvette.chambers@ik.me
-- Stability   : experimental
--
-- A composable, extensible multi-context static Reader effect for @effectful@.
--
-- Instead of maintaining multiple distinct 'Reader' effects in the effect stack,
-- which incur multiple handler allocations and separate environment lookups, 'Soup'
-- packs any number of labeled environments into a single flat contiguous 'Record'
-- backed by Polytropos's 'Rep'.
--
-- Key Properties:
--
-- * __O(1) Direct Indexing__: Retrieval via 'askSoup' resolves at compile time
--   to a direct literal slot offset synthesized by the Polytropos type-checker plugin.
-- * __O(K) Sub-Soup Slicing__: Computations requiring only a subset of contexts can use
--   'filtering' to narrow the active environment in $O(K)$ time via 'SubRow' evidence.
-- * __Interoperability__: Functions written for standard 'Reader' or 'Labeled' effects can
--   be directly executed within 'Soup' using 'withSoupLabel' or 'withSoupAsLabeled'.
module Effectful.Soup
  ( -- * Effect
    Soup
  , ReaderSoup

    -- ** Handlers
  , runSoup
  , runReaderSoup

    -- ** Operations
  , askSoup
  , askSoupIn
  , asksSoup
  , localSoup
  , filtering
  , FindSoup

    -- ** Interoperability
  , withSoupLabel
  , withSoupAsLabeled

    -- ** Re-exports from Polytropos
  , Field(..)
  , FieldIndex
  , SubRow
  , Record
  , Pair(..)
  , (:::)
  , HasEffect
  ) where

import Data.Functor.Identity (Identity(..))
import Data.Kind (Type)
import Data.Primitive.SmallArray (indexSmallArray)
import GHC.TypeLits (Symbol)
import Unsafe.Coerce (unsafeCoerce)

import Effectful
import Effectful.Dispatch.Static
import Effectful.Internal.Effect ((:::), HasEffect)
import Effectful.Labeled (Labeled, runLabeled)
import Effectful.Reader.Dynamic qualified as RD
import Effectful.Reader.Static (Reader, runReader)

import Polytropos (Record, projectRecord, setField)
import Polytropos.Core
  ( Field(..), FieldIndex(..), Pair(..), Rep(..), RowRep(..)
  , SubRow
  )
import Polytropos.ReaderSoup (LookupContext)

-- | Fetch a field value from a 'Record' using its compile-time synthesized index.
getField :: forall n a r. FieldIndex n r a => Field n -> Record r -> a
getField _ (RowRep (Rep _ v)) =
  case indexSmallArray v (fieldIndex @_ @n @r @a) of
    Identity val -> unsafeCoerce val

-- | A composite, static Reader soup effect holding a flat contiguous 'Record'
-- of labeled contexts.
data Soup (ctxs :: [Pair Symbol Type]) :: Effect

-- | Alias for 'Soup'.
type ReaderSoup = Soup

type instance DispatchOf (Soup ctxs) = Static NoSideEffects
newtype instance StaticRep (Soup ctxs) = Soup (Record ctxs)

-- | Run a 'Soup' effect by supplying the context 'Record'.
runSoup :: Record ctxs -> Eff (Soup ctxs : es) a -> Eff es a
runSoup rec = evalStaticRep (Soup rec)

-- | Alias for 'runSoup'.
runReaderSoup :: Record ctxs -> Eff (ReaderSoup ctxs : es) a -> Eff es a
runReaderSoup = runSoup

-- | Locate the ambient 'Soup' context row in the effect stack.
type family FindSoup (es :: [Effect]) :: [Pair Symbol Type] where
  FindSoup (Soup ctxs : _) = ctxs
  FindSoup (_ : es)        = FindSoup es

-- | Retrieve a specific context @c@ labeled @l@ from the ambient soup in $O(1)$ time.
--
-- Example:
--
-- >>> askSoup #db
-- >>> askSoup (Field @"logger")
askSoup
  :: forall l es c.
     ( c ~ LookupContext l (FindSoup es)
     , FieldIndex l (FindSoup es) c
     , Soup (FindSoup es) :> es
     )
  => Field l
  -> Eff es c
askSoup fld = do
  Soup rec <- getStaticRep @(Soup (FindSoup es))
  pure $ getField fld rec

-- | Retrieve a specific context @c@ labeled @l@ from a specific soup @ctxs@.
askSoupIn
  :: forall ctxs l c es.
     ( c ~ LookupContext l ctxs
     , FieldIndex l ctxs c
     , Soup ctxs :> es
     )
  => Field l
  -> Eff es c
askSoupIn fld = do
  Soup rec <- getStaticRep @(Soup ctxs)
  pure $ getField fld rec

-- | Retrieve a projection of a context @c@ from the ambient soup.
--
-- @'asksSoup' fld f ≡ f '<$>' 'askSoup' fld@
asksSoup
  :: forall l es c a.
     ( c ~ LookupContext l (FindSoup es)
     , FieldIndex l (FindSoup es) c
     , Soup (FindSoup es) :> es
     )
  => Field l
  -> (c -> a)
  -> Eff es a
asksSoup fld f = f <$> askSoup @l fld

-- | Locally update a specific field in the ambient soup for the scope of an inner action.
localSoup
  :: forall l es c a.
     ( c ~ LookupContext l (FindSoup es)
     , FieldIndex l (FindSoup es) c
     , Soup (FindSoup es) :> es
     )
  => Field l
  -> (c -> c)
  -> Eff es a
  -> Eff es a
localSoup fld f act = do
  Soup rec <- getStaticRep @(Soup (FindSoup es))
  let !old = getField fld rec
      !new = f old
      !rec' = setField fld new rec
  localStaticRep @(Soup (FindSoup es)) (\_ -> Soup rec') act

-- | Restrict the ambient soup to a sub-row @sub@ in $O(K)$ time via 'SubRow'.
filtering
  :: forall sub es a.
     ( SubRow sub (FindSoup es)
     , Soup (FindSoup es) :> es
     )
  => Eff (Soup sub : es) a
  -> Eff es a
filtering act = do
  Soup superRec <- getStaticRep @(Soup (FindSoup es))
  let !subRec = projectRecord superRec
  runSoup subRec act

-- | Run an unlabelled 'Reader c' action by dipping into context @l@ of the ambient soup.
withSoupLabel
  :: forall l es c a.
     ( c ~ LookupContext l (FindSoup es)
     , FieldIndex l (FindSoup es) c
     , Soup (FindSoup es) :> es
     )
  => Field l
  -> Eff (Reader c : es) a
  -> Eff es a
withSoupLabel fld act = do
  val <- askSoup @l fld
  runReader val act

-- | Run a 'Labeled l (RD.Reader c)' action by dipping into context @l@ of the ambient soup.
withSoupAsLabeled
  :: forall l es c a.
     ( c ~ LookupContext l (FindSoup es)
     , FieldIndex l (FindSoup es) c
     , Soup (FindSoup es) :> es
     )
  => Field l
  -> Eff (Labeled l (RD.Reader c) : es) a
  -> Eff es a
withSoupAsLabeled fld act = do
  val <- askSoup @l fld
  runLabeled @l (RD.runReader val) act
