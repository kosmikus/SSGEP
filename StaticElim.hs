{-# LANGUAGE DataKinds, KindSignatures, GADTs, RankNTypes, StandaloneDeriving, ScopedTypeVariables #-}
module StaticElim where

-- Preliminaries
data Nat = Zero | Suc Nat

data Vec (a :: *) (n :: Nat) where
  VNil   ::  Vec a Zero
  VCons  ::  a -> Vec a n -> Vec a (Suc n)

infixr 5 `VCons`

deriving instance Show a => Show (Vec a n)

-- Implicit singleton defined in final style.
class SNatI (n :: Nat) where
  -- This function can be used to make a static choice.
  sNatStatic :: r Zero -> (forall n . r n -> r (Suc n)) -> r n

instance SNatI Zero where
  sNatStatic zero suc = zero

instance SNatI n => SNatI (Suc n) where
  sNatStatic zero suc = suc (sNatStatic zero suc)

-- Explicit singleton.
--
-- This is just like in the lecture, but with the
-- different SNatI class.

data SNat (n :: Nat) where
  SZero  ::  SNat Zero
  SSuc   ::  SNatI n => SNat (Suc n)

-- Helper datatype
data WrappedSNat n where
  WrappedSNat :: SNatI n => SNat n -> WrappedSNat n

unwrap :: WrappedSNat n -> SNat n
unwrap (WrappedSNat x) = x

-- This can be used to make a dynamic choice. 
sNatDynamic :: SNatI n => SNat n
sNatDynamic = unwrap $ sNatStatic
                         (WrappedSNat SZero)
                         (\ (WrappedSNat _) -> WrappedSNat SSuc)

-- Static
vreplicateS :: SNatI n => a -> Vec a n
vreplicateS x = sNatStatic VNil (x `VCons`)

-- Dynamic
vreplicateD :: forall n a . SNatI n => a -> Vec a n
vreplicateD x = case sNatDynamic :: SNat n of
  SZero  ->  VNil
  SSuc   ->  x `VCons` vreplicateD x

testS :: Vec Char (Suc (Suc (Suc Zero)))
testS = vreplicateS 'x'

testD :: Vec Char (Suc (Suc (Suc Zero)))
testD = vreplicateD 'x'

-- Simplified Core for testS:
--
-- $WVNil
-- $WVNil = \ @ a_aqi -> VNil @~ <'Zero>_N
--
-- a2_r1a1
-- a2_r1a1 = C# 'x'
-- 
-- a3_r1a2
-- a3_r1a2 = VCons @~ <'Suc 'Zero>_N a2_r1a1 ($WVNil)
-- 
-- a4_r1a3
-- a4_r1a3 = VCons @~ <'Suc ('Suc 'Zero)>_N a2_r1a1 a3_r1a2
-- 
-- testS
-- testS = VCons @~ <'Suc ('Suc ('Suc 'Zero))>_N a2_r1a1 a4_r1a3
-- 

-- Simplified Core for testD:
--
-- testD
-- testD = vreplicateD (a1_r1a0 `cast` ...) (C# 'x')
-- 
-- Rec {
-- vreplicateD
-- vreplicateD =
--   \ @ n_a15J @ a5_a15K $dSNatI_a174 x_aqA ->
--     case sNatDynamic $dSNatI_a174 of _ {
--       SZero dt_d19w -> ($WVNil) `cast` ...;
--       SSuc @ n1_a17e dt_d19s $dSNatI1_a17g ->
--         (VCons @~ <'Suc n1_a17e>_N x_aqA (vreplicateD $dSNatI1_a17g x_aqA))
--         `cast` ...
--     }
-- end Rec }
-- 
