module Primitives where

import BaseTypes
import Data.Vector.Class
import Data.Vector.V3

threshold = 0.001 :: Double

solveq :: (Double, Double, Double) ->[Double]
solveq (a,b,c)
     | d < 0 = []
     | d > 0 = [(- b - sqrt d)/(2*a), (- b + sqrt d)/(2*a)]
     | otherwise = [-b/(2*a)]
     where
        d = b*b - 4*a*c

data Sphere = Sphere { sphere_center :: Vector3, radius :: Double }

instance FigureType Sphere where
    normal_at (Sphere center _) point = vnormalise (point - center)

    hit ray@(Ray pos dir) sphere@(Sphere center radius)
        | roots == [] = Nothing
        | distance < threshold = Nothing
        | otherwise = Just (hit_pos, local_hit_pos)
            where d = (pos - center)
                  roots = filter (> threshold) (solveq (vdot dir dir, 2 * (vdot dir d), (vdot d d) - radius^2))
                  distance = minimum roots
                  hit_pos = pos + (distance *| dir)
                  local_hit_pos = hit_pos - center

data Plane = Plane { plane_center, normal :: Vector3 }

instance FigureType Plane where
    normal_at (Plane _ normal) _ = normal

    hit ray@(Ray pos dir) plane@(Plane center normal)
        | abs part < (10**(-5)) = Nothing
        | distance < threshold = Nothing
        | otherwise = Just (hit_pos, hit_pos)
            where part = vdot dir normal
                  distance = (vdot (center - pos) normal) / part
                  hit_pos = pos + (distance *| dir)
