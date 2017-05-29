module Pendulum.View (
  drawPendulum
  ) where

import qualified Data.Vector.Sized as V
import qualified Data.Vector.Storable as VS
import           GHC.TypeLits
import           Graphics.Gloss
import           Numeric.Hamilton
import           Numeric.LinearAlgebra.Static
import           Pendulum.Model (Pendulum(..))


-- | 振り子の長さの描画スケール
lengthScale :: Float
lengthScale = 100.0

-- | 振り子のおもりの描画スケール
massScale :: Float
massScale = 10.0

r2list :: KnownNat n => R n -> [Double]
r2list = VS.toList . extract

drawPendulum :: KnownNat n => Pendulum n -> Picture
drawPendulum pd =
  let ls  = map ((*lengthScale) . realToFrac) (V.toList $ _lengthes pd)
      ms  = map ((*massScale) . realToFrac) (V.toList $ _masses pd)
      ps  = map realToFrac (r2list (cfgPositions (_config pd)))
      ths = map (subtract (pi / 2.0)) ps
      xs = scanl (+) 0 (zipWith (*) ls (map cos ths))
      ys = scanl (+) 0 (zipWith (*) ls (map sin ths))
      lines   = line (zip xs ys)
      circles = zipWith3 (translate) (tail xs) (tail ys) (map circleSolid ms)
   in pictures (lines:circles)


