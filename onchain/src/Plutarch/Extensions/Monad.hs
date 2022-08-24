{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Extensions.Monad (pletFieldC, pmatchFieldC, pifC) where

import Plutarch.Prelude

import GHC.TypeLits (KnownNat)
import Plutarch.DataRepr (PDataFields (PFields))
import Plutarch.DataRepr.Internal (PLabelIndex, PUnLabel)
import Plutarch.DataRepr.Internal.HList (IndexList)
import Plutarch.Extra.TermCont (pletC, pmatchC)

pletFieldC ::
  forall name p s a as n.
  ( PDataFields p
  , as ~ PFields p
  , n ~ PLabelIndex name as
  , KnownNat n
  , a ~ PUnLabel (IndexList n as)
  , PIsData a
  ) =>
  Term s p ->
  TermCont s (Term s a)
pletFieldC t = pletC $ pfield @name # t

pmatchFieldC ::
  forall name p s a as n.
  ( PDataFields p
  , as ~ PFields p
  , n ~ PLabelIndex name as
  , KnownNat n
  , a ~ PUnLabel (IndexList n as)
  , PIsData a
  , PMatch a
  ) =>
  Term s p ->
  TermCont s (a s)
pmatchFieldC t = pmatchC $ pfield @name # t

pifC :: forall s a. Term s PBool -> TermCont @a s (Term s a) -> TermCont @a s (Term s a) -> TermCont @a s (Term s a)
pifC cond whenTrue whenFalse =
  pure $ pif cond (unTermCont whenTrue) (unTermCont whenFalse)

-- TODO is one of these better than the other?
{-
pifC cond whenTrue whenFalse = pmatchC cond >>= \case
  PTrue -> whenTrue
  PFalse -> whenFalse
-}
