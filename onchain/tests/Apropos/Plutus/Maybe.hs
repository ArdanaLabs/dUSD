module Apropos.Plutus.Maybe (
  MaybeProp (..),
) where

import Apropos
import Control.Lens (_Just)


data MaybeProp p
  = IsNothing
  | IsJust
  | JustAnd p
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable, Hashable)

instance LogicalModel p => LogicalModel (MaybeProp p) where
  logic =
    ExactlyOne [Var IsNothing, Var IsJust]
      -- TODO type ambiguity makes it hard to use abstractionLogic here
      -- FunctionDependencies should fix this?
      :&&: (Var IsNothing :->: None (Var . JustAnd <$> enumerated))
      :&&: (Var IsJust :->: JustAnd <$> logic)

instance HasLogicalModel p m => HasLogicalModel (MaybeProp p) (Maybe m) where
  satisfiesProperty IsNothing Nothing = True
  satisfiesProperty IsNothing (Just _) = False
  satisfiesProperty IsJust (Just _) = True
  satisfiesProperty IsJust Nothing = False
  satisfiesProperty (JustAnd _) Nothing = False
  satisfiesProperty (JustAnd p) (Just m) = satisfiesProperty p m

instance
  ( HasParameterisedGenerator p m
  , HasPermutationGenerator p m
  ) =>
  HasAbstractions (MaybeProp p) (Maybe m)
  where
  abstractions =
    [ WrapAbs $
        SumAbstraction
          { abstractionName = "Just"
          , propertyAbstraction = abstractsProperties JustAnd
          , propLabel = IsJust
          , sumModelAbstraction = _Just
          }
    ]

instance
  ( HasParameterisedGenerator p m
  , HasPermutationGenerator p m
  ) =>
  HasPermutationGenerator (MaybeProp p) (Maybe m)
  where
  generators =
    abstractionMorphisms
      ++ [ Morphism
            { name = "make Nothing"
            , contract = clear >> add IsNothing
            , match = Yes
            , morphism = const $ pure Nothing
            }
         ]

instance
  ( HasParameterisedGenerator p m
  , HasPermutationGenerator p m
  ) =>
  HasParameterisedGenerator (MaybeProp p) (Maybe m)
  where
  parameterisedGenerator = buildGen $ choice [pure Nothing, Just <$> genSatisfying @p @m Yes]
