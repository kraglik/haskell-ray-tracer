module Main where

import Data.Vector.Class
import Data.Vector.V3
import BaseTypes
import LocalShadingModels
import LightSources
import Primitives
import Textures
import ViewingSystem
import WhittedTracer
import Samplers
import BaseFuncs

size = 1000

sphere_texture = Texture (ConstColor (Color 1 0 0))
sphere_mat = Material 0.1 0.25 0.35 0.2 0 8 0 sphere_texture
sphere = Figure (Sphere (Vector3 4 1 0) 1.0)

glass_texture = Texture (ConstColor (Color 0.8 0.8 0.8))
glass = Material 0.0 0.0 0.0 0.0 1.0 15.0 2.0 glass_texture
glass_sphere = Figure (Sphere (Vector3 2 1 (-1)) 0.4)

plane_texture = Texture (ChessBoard (Color 1 1 1) (Color 0 0 0) 1.0)
plane_mat = Material 0.1 0.6 0.3 0 0 3 0 plane_texture
plane = Figure (Plane (Vector3 1 (-0.1) 0) (Vector3 0 1 0))

sun = LightSource (DirectedLight (vnormalise (Vector3 (0.4) (-1) (-0.3))) (Color 1 1 1))
lamp = LightSource (PointLight (Vector3 2.0 1.0 (-1.0)) (Color 1.0 1.0 1.0))
ambient_light = AmbientLight (Color 0.2 0.4 0.8)

test_scene = Scene  [sun]
                    [(plane, plane_mat), (sphere, sphere_mat), (glass_sphere, glass)]
                    ambient_light

tracer = Tracer (WhittedTracer (ShadingModel LambertModel) (ShadingModel PhongModel) 4 test_scene)

regular_sampler = Sampler (init_regular_sampler 9 1.0)

vp = ViewPlane 1.0 1.0 1.0 size size regular_sampler
camera = PinholeCamera (init_camera_vectors (Vector3 0 1.5 0) (Vector3 3.0 0 0) (Vector3 0 1 0)) vp

make_pgm :: Integer -> Integer -> [ Color ] -> String
make_pgm width height xs = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n" ++ stringify(xs)
                  where stringify [] = ""
                        stringify ((Color r g b):xs) = show (round (r*255)) ++ " "
                                                 ++ show (round (g*255)) ++ " "
                                                 ++ show (round (b*255)) ++ " "
                                                 ++ stringify xs

rows = render_scene camera tracer
rendered = map clip (foldr (++) [] (reverse rows))

main = writeFile "result.pgm" (make_pgm size size rendered)
