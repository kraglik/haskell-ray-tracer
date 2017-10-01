module LightSources where

import BaseTypes
import Data.Vector.V3
import Data.Vector.Class
import Data.Maybe

data PointLight = PointLight { position :: Vector3, pointed_color :: Color }

data DirectedLight = DirectedLight { direction :: Vector3, directed_color :: Color }

hit_scene :: Ray -> Scene -> [Material]
hit_scene ray@(Ray o d) scene = [mt | (f, mt) <- (figures scene), isJust (hit new_ray f)]
    where new_ray = Ray (o + (d |* 0.001)) d

{-# INLINE hit_scene #-}

hit_range :: Ray -> Scene -> Scalar -> [Material]
hit_range ray scene max = [mt | (f, mt) <- figures scene, in_range f]
    where in_range f = isJust h && vmag (origin ray - fst (fromJust h)) < (max - 0.001)
            where h = hit ray f

{-# INLINE hit_range #-}

instance LightType PointLight where
    direction_at (PointLight pos _) point = vnormalise (pos - point)

    color (PointLight _ col) = col

    trace_shadow (PointLight pos _) point scene =
            hit_range (Ray point (vnormalise (pos - point))) scene (vmag (pos - point))

    force_at (PointLight pos _) point = 1.0 / ((vmag (pos - point))^2)

instance LightType DirectedLight where
    color (DirectedLight _ col) = col

    direction_at (DirectedLight dir _) _ = dir

    trace_shadow (DirectedLight dir _) point scene = hit_scene (Ray point (negate dir)) scene

    force_at (DirectedLight _ _) _ = 1.0
