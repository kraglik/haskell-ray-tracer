module LocalShadingModels where

import BaseTypes
import BaseFuncs
import Data.Maybe
import Data.Vector.Class
import Data.Vector.V3

data LambertModel = LambertModel
data PhongModel = PhongModel
data BlinnPhongModel = BlinnPhongModel

instance ShadingModelType LambertModel where
    shade LambertModel hit tracer = (foldr (+) (Color 0 0 0) lights_colors)
     where hit_color = color_at_hit hit
           (norm, hit_point) = (normal hit, hit_pos hit)
           visible = filter (\(n, _, _) -> isJust n) [(cast_shadow tracer (hit_pos hit) x, negate (direction_at x hit_point), force_at x hit_point)
                                                  | x <- (lights (scene tracer))]
           dots = [vdot norm x| (_, x, _) <- visible]
           calculate_color i = (fromJust c) *. multiplier where
                                 tuple@(c, d, f) = visible !! i
                                 multiplier = (dots !! i) * f
           lights_colors = [calculate_color i | i <- [0..length dots - 1], dots !! i > 0]

instance ShadingModelType PhongModel where
    shade PhongModel hit tracer = (foldr (+) (Color 0 0 0) lights_colors)
     where hit_color = color_at_hit hit
           (norm, hit_point, nrdir) = (normal hit, hit_pos hit, negate $ direction (ray hit))
           visible = filter (\(n, _, _) -> isJust n)  [(cast_shadow tracer (hit_pos hit) x, reflect (direction_at x hit_point) norm, force_at x hit_point)
                                                   | x <- (lights (scene tracer))]
           dots = [vdot dir nrdir | (_, dir, _) <- visible]
           calculate_color i = (fromJust c) *. multiplier where
                                tuple@(c, d, f) = visible !! i
                                multiplier = (dots !! i) * f
           lights_colors = [calculate_color i | i <- [0..length dots - 1], dots !! i > 0]

instance ShadingModelType BlinnPhongModel where
    shade BlinnPhongModel hit tracer = (foldr (+) (Color 0 0 0) lights_colors)
     where hit_color = color_at_hit hit
           (norm, hit_point, nrdir) = (normal hit, hit_pos hit, negate $ direction (ray hit))
           visible = filter (\(n, _, _) -> isJust n)  [(cast_shadow tracer (hit_pos hit) x, direction_at x hit_point, force_at x hit_point)
                                                   | x <- (lights (scene tracer))]
           dirs = [(nrdir + (negate dir)) |/ 2.0 | (_, dir, _) <- visible]
           dots = [vdot x norm | x <- dirs]
           calculate_color i = (fromJust c) *. multiplier where
                                 tuple@(c, d, f) = visible !! i
                                 multiplier = (dots !! i) * f
           lights_colors = [calculate_color i | i <- [0..length dots - 1], dots !! i > 0]

