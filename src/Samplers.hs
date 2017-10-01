module Samplers where

import BaseTypes
import System.Random
import Data.Vector.Class
import Data.Vector.V3

data RegularSampler = RegularSampler [Vector3] [Vector3] [Vector3]

init_regular_sampler :: Integer -> Double -> RegularSampler
init_regular_sampler sc exp = RegularSampler samples disk_samples hemisphere_samples
        where fsc = fromIntegral sc
              fsc_root = round (sqrt fsc)
              samples = [Vector3 ((fromIntegral x + 0.5) / fsc) ((fromIntegral y + 0.5) / fsc) 0
                        | x <- [0..fsc_root], y <- [0..fsc_root] ]
              disk_samples = map (\x -> map_sample x exp) samples
              hemisphere_samples = map_hemisphere samples exp

map_sample :: Vector3 -> Double -> Vector3
map_sample point@(Vector3 x y z) exp = Vector3 (sin_theta * cos_phi) (sin_theta * sin_phi) cos_theta
    where
        cos_phi = cos (2.0 * pi * x)
        sin_phi = sin (2.0 * pi * x)
        cos_theta = if y == 0 then (1.0 - y) ** (1.0 / (exp + 1.0)) else 0.0
        sin_theta = sqrt (abs (1.0 - cos_theta * cos_theta))

map_hemisphere :: [Vector3] -> Double -> [Vector3]
map_hemisphere samples exp = [map_sample x exp | x <- samples]

instance SamplerType RegularSampler where
    square_samples (RegularSampler samples _ _) = samples
    disk_samples (RegularSampler _ samples _) = samples
    hemisphere_samples (RegularSampler _ _ samples) = samples




