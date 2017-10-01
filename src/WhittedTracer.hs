module WhittedTracer where

import BaseTypes
import BaseFuncs
import Data.Vector.Class
import Data.Vector.V3
import Data.Maybe

------------------------------------------------------------------------------------------------------------------------

data WhittedTracer = WhittedTracer
                   {
                       diffuse_model, specular_model :: ShadingModel, -- local shading models needed
                       max_depth :: Integer,
                       whitted_scene :: Scene
                   }

instance TracerType WhittedTracer where
    trace tracer@(WhittedTracer diffuse specular max_depth scene) depth ray@(Ray orig dir)
        | depth > max_depth = Color 0.0 0.0 0.0
        | null hits = color (ambient scene)
        | otherwise = hit_color * (ambient_color + diffuse_color + specular_color + reflection + refraction)
        where
            hits = map fromJust (filter isJust [make_hit x ray | x <- figures scene])
            nearest = nearest_hit orig hits (head hits)
            mt = material nearest
            hit_color = color_at_hit nearest
            ambient_color = (ka mt) .* color (ambient scene)
            diffuse_color = (kd mt) .* shade diffuse nearest tracer
            specular_color = (ks mt) .* shade specular nearest tracer
            reflection = if (kr mt) > 0
                            then trace tracer (depth + 1) (Ray (hit_pos nearest) (reflect dir (normal nearest)))
                            else Color 0 0 0
            refraction = if (kt mt) > 0
                            then trace tracer (depth + 1) (Ray (hit_pos nearest) (refract dir (normal nearest) (ior mt)))
                            else Color 0 0 0

    cast_shadow (WhittedTracer diffuse specular max_depth scene) point light
        | length materials > length transparent = Nothing
        | null materials = Just (color light)
        | otherwise = Just ((color light) *. multiplier)
        where
            materials = trace_shadow light point scene
            transparent = filter (\x -> (kt x) > 0) materials
            multiplier = (foldr (+) 0 (map kt transparent)) / (fromIntegral (length transparent))

    scene (WhittedTracer _ _ _ s) = s

------------------------------------------------------------------------------------------------------------------------

make_hit :: (Figure, Material) -> Ray -> Maybe Hit
make_hit (f, m) ray
    | isNothing points = Nothing
    | otherwise = Just (Hit global local m ray n)
        where points = hit ray f
              (global, local) = (fst (fromJust points), snd (fromJust points))
              n = normal_at f global

nearest_hit :: Vector3 -> [Hit] -> Hit -> Hit
nearest_hit point hits current
    | null hits = current
    | vmag (point - (hit_pos h)) < vmag (point - (hit_pos current)) = nearest_hit point (tail hits) h
    | otherwise = nearest_hit point (tail hits) current
        where h = head hits
