module BaseFuncs where

import BaseTypes
import Data.Vector.Class
import Data.Vector.V3

reflect :: Vector3 -> Vector3 -> Vector3
reflect dir norm = vnormalise $ dir - 2 *| (norm |* (vdot dir norm))

{-# INLINE reflect #-}

refract :: Vector3 -> Vector3 -> Double -> Vector3
refract dir norm ior
    | nv > 0 = refract dir ((-1) *| norm) a
    | d < 0 = Vector3 0 0 0
    | otherwise = (a *| dir) - (b *| norm)
    where nv = vdot norm dir
          a = 1.0 / ior
          d = 1.0 - (a * a) * (1.0 - nv * nv)
          b = nv * a + sqrt d

{-# INLINE refract #-}

clip :: Color -> Color
clip (Color x y z) = Color     (max (min 1.0 x) 0.0)
                               (max (min 1.0 y) 0.0)
                               (max (min 1.0 z) 0.0)

{-# INLINE clip #-}
