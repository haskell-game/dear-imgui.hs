{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module DearImGui.Enums where

-- base
import GHC.Exts
  ( proxy# )
import GHC.TypeNats
  ( Nat, KnownNat, natVal' )
import Numeric.Natural
  ( Natural )

-- dear-imgui-generator
import DearImGui.Generator
  ( declareEnumerations )

--------------------------------------------------------------------------------

class KnownNat ( Count a ) => FiniteEnum a where
  type Count a :: Nat
  count :: Natural
  count = natVal' @( Count a ) proxy#

declareEnumerations ''FiniteEnum ''Count
