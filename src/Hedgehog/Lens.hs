{-# LANGUAGE RankNTypes #-}

module Hedgehog.Lens
  ( wellFormedPrism,
    wellFormedLens,
    wellFormedIso,
    prismExample,
  )
where

import Control.Lens
  ( Iso',
    Lens',
    Prism',
    matching,
    preview,
    review,
    set,
    view,
  )
import Control.Monad (Monad (return))
import Data.Either (Either (Left, Right))
import Data.Eq (Eq)
import Data.Function ((.))
import Data.Maybe (Maybe (Just))
import Hedgehog (Gen, PropertyT, annotate, forAll, (===))
import Text.Show (Show)

-- | Checks whether a prism respects the well-formedness laws:
--
-- 1) Get-Set Prism Law
-- 2) Set-Get Prism Law
wellFormedPrism ::
  Monad m =>
  (Show large, Eq large) =>
  (Show small, Eq small) =>
  Gen large ->
  Gen small ->
  -- | Prism signifying that the
  --          @small@ type is a subset of the @large@ type
  Prism' large small ->
  PropertyT m ()
wellFormedPrism genLarge genSmall o =
  do
    getSetPrismLaw genLarge o
    setGetPrismLaw genSmall o

-- | Get-Set Prism Law
--
-- @
-- 'matching' o s ≡ 'Right' a => 'review' o a ≡ s
-- @
getSetPrismLaw ::
  Monad m =>
  (Show large, Eq large) =>
  (Show small, Eq small) =>
  Gen large ->
  Prism' large small ->
  PropertyT m ()
getSetPrismLaw genLarge o =
  do
    large <- forAll genLarge
    case matching o large of
      Right small ->
        do
          annotate "The get-set law must hold for a Prism"
          review o small === large
      Left _ -> return ()

-- | Set-Get Prism Law
--
-- @
-- 'matching' o ('review' o b) ≡ 'Right' b
-- @
setGetPrismLaw ::
  Monad m =>
  (Show large, Eq large) =>
  (Show small, Eq small) =>
  Gen small ->
  Prism' large small ->
  PropertyT m ()
setGetPrismLaw genSmall o =
  do
    small <- forAll genSmall
    annotate "The set-get law must hold for a Prism"
    matching o (review o small) === Right small

-- | Checks whether a lens respects the well-formedness laws:
--
-- 1) Get-Put Lens Law
-- 2) Put-Get Lens Law
-- 3) Put-Put Lens Law
wellFormedLens ::
  Monad m =>
  (Show large, Eq large) =>
  (Show small, Eq small) =>
  Gen large ->
  Gen small ->
  -- | Lens signifying that the @small@
  --          type is a constituent part of the @large@ type
  Lens' large small ->
  PropertyT m ()
wellFormedLens genLarge genSmall o =
  do
    getPutLensLaw genLarge genSmall o
    putGetLensLaw genLarge o
    putPutLensLaw genLarge genSmall o

-- | Get-Put Lens Law
-- You get back what you put in:
--
-- @
-- 'view' l ('set' l v s) ≡ v
-- @
getPutLensLaw ::
  Monad m =>
  (Show large, Eq large) =>
  (Show small, Eq small) =>
  Gen large ->
  Gen small ->
  Lens' large small ->
  PropertyT m ()
getPutLensLaw genLarge genSmall o =
  do
    large <- forAll genLarge
    small <- forAll genSmall
    annotate "The set-get law must hold for a Lens"
    view o (set o small large) === small

-- | Put-Get Lens Law
-- Putting back what you got doesn't change anything:
--
-- @
-- 'set' l ('view' l s) s ≡ s
-- @
putGetLensLaw ::
  Monad m =>
  (Show large, Eq large) =>
  (Show small, Eq small) =>
  Gen large ->
  Lens' large small ->
  PropertyT m ()
putGetLensLaw genLarge o =
  do
    large <- forAll genLarge
    annotate "The get-set law must hold for a Lens"
    set o (view o large) large === large

-- | Put-Put Lens Law
-- Setting twice is the same as setting once:
--
-- @
-- 'set' l v' ('set' l v s) ≡ 'set' l v' s
-- @
putPutLensLaw ::
  Monad m =>
  (Show large, Eq large) =>
  (Show small, Eq small) =>
  Gen large ->
  Gen small ->
  Lens' large small ->
  PropertyT m ()
putPutLensLaw genLarge genSmall o =
  do
    large <- forAll genLarge
    small1 <- forAll genSmall
    small2 <- forAll genSmall
    annotate "The set-set law must hold for a Lens"
    set o small2 (set o small1 large) === set o small2 large

-- | Checks whether an isomorphism respects the well-formedness laws:
--
-- 1) Set-Get Iso Law
-- 2) Get-Set Iso Law
wellFormedIso ::
  Monad m =>
  (Show a, Eq a) =>
  (Show b, Eq b) =>
  Gen a ->
  Gen b ->
  -- | Isomorphism signifying that types
  --          @a@ and @b@ are basically the same thing
  Iso' a b ->
  PropertyT m ()
wellFormedIso genA genB o =
  do
    setGetIsoLaw genB o
    getSetIsoLaw genA o

-- | Set-Get Iso Law
-- You get back what you put in:
--
-- @
-- 'view' i . 'review' i ≡ 'id'
-- @
setGetIsoLaw ::
  Monad m =>
  (Show a, Eq a) =>
  (Show b, Eq b) =>
  Gen b ->
  Iso' a b ->
  PropertyT m ()
setGetIsoLaw genB o =
  do
    b <- forAll genB
    annotate "The set-get law must hold for an Iso"
    (view o . review o) b === b

-- | Get-Set Iso Law
-- You get back what you put in:
--
-- @
-- 'review' i . 'view' i ≡ 'id'
-- @
getSetIsoLaw ::
  Monad m =>
  (Show a, Eq a) =>
  (Show b, Eq b) =>
  Gen a ->
  Iso' a b ->
  PropertyT m ()
getSetIsoLaw genA o =
  do
    a <- forAll genA
    annotate "The get-set law must hold for an Iso"
    (review o . view o) a === a

-- | Assert that a prism matches for a particular set of values:
-- A 'review' of the @small@ value should produce the @large@ value, and
-- a 'preview' of the @large@ value should produce the @small@ value.
prismExample ::
  Monad m =>
  (Show large, Eq large) =>
  (Show small, Eq small) =>
  -- | Prism signifying that the
  --          @small@ type is a subset of the @large@ type
  Prism' large small ->
  large ->
  small ->
  PropertyT m ()
prismExample o large small =
  do
    review o small === large
    preview o large === Just small
