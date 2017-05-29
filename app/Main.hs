{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import qualified Data.Vector.Sized as V
import           GHC.TypeLits (KnownNat)
import           Graphics.Gloss
import           Numeric.Hamilton
import           Numeric.LinearAlgebra.Static
import           Pendulum.Model
import           Pendulum.View

toVector :: forall a m. KnownNat m => [a] -> V.Vector m a
toVector = (\(Just x) -> x) . V.fromList

pdConfig :: Config 4
pdConfig =
  let th = 0.5 * pi
   in Cfg (vector (replicate 4 th)) (vector (replicate 4 0))

pendulum :: Pendulum 4
pendulum = Pendulum
  { _lengthes = toVector (replicate 4 0.8)
  , _masses   = toVector (replicate 4 0.8)
  , _config   = pdConfig
  }

pdSystem :: System 8 4
pdSystem = mkPendlumSystem pendulum

main :: IO ()
main = do
  let display = InWindow "Multi Pendulum Simulation" (800, 800) (10, 10)
  simulate display white 30 pendulum drawPendulum (\_ dt pd -> stepPendulum (realToFrac dt) pdSystem pd)
