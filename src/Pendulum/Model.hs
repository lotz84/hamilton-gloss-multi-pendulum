{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pendulum.Model (
  Pendulum(..),
  mkPendlumSystem,
  stepPendulum
  ) where

import qualified Data.Vector.Sized as V
import           GHC.TypeLits (KnownNat)
import           Numeric.Hamilton
import           Numeric.LinearAlgebra.Static

data Pendulum n = Pendulum
  { _lengthes :: V.Vector n Double
  , _masses   :: V.Vector n Double
  , _config   :: Config n
  }

gravity :: Floating a => a
gravity = 9.8

toFloating :: Floating a => Double -> a
toFloating = fromRational . toRational

duplicate :: [a] -> [a]
duplicate = concatMap (\x -> [x, x])

merge :: [a] -> [a] -> [a]
merge []     ys     = ys
merge xs     []     = xs
merge (x:xs) (y:ys) = x : y : merge xs ys

evenElems :: [a] -> [a]
evenElems []       = []
evenElems (_:[])   = []
evenElems (_:x:xs) = x : evenElems xs

toVector :: forall a m. KnownNat m => [a] -> V.Vector m a
toVector = (\(Just x) -> x) . V.fromList

mkPendlumSystem :: forall m n. (KnownNat m, KnownNat n) => Pendulum n -> System m n
mkPendlumSystem p =
  let lengthes = V.toList (_lengthes p)
      masses   = V.toList (_masses p)
      masses' :: R m
      masses' = vector (duplicate masses)
      coordinates :: Floating a => V.Vector n a -> V.Vector m a
      coordinates v =
        let ths = V.toList v
            ls = map toFloating lengthes
            xs =                scanl1 (+) (zipWith (*) ls (map sin ths))
            ys = (* (-1.0)) <$> scanl1 (+) (zipWith (*) ls (map cos ths))
         in toVector (merge xs ys)
      potential :: Floating a => V.Vector m a -> a
      potential v =
        let ys = evenElems (V.toList v)
            ms = map toFloating masses
         in gravity * foldl1 (+) (zipWith (*) ms ys)
   in mkSystem' masses' coordinates potential

stepPendulum :: forall m n. (KnownNat m, KnownNat n) => Double -> System m n -> Pendulum n -> Pendulum n
stepPendulum dt system pd =
  let phase  = toPhase system (_config pd)
      phase' = stepHam dt system phase
   in pd {_config = fromPhase system phase'}

