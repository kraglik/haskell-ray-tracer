module ViewingSystem where

import BaseTypes
import Data.Vector.Class
import Data.Vector.V3

data ViewPlane = ViewPlane
                {
                    vp_distance, ratio, pixel_size :: Double,
                    width, height :: Integer,
                    sampler :: Sampler
                }

data CameraVectors = CameraVectors  -- basic and most common camera information like position, direction, e.t.c.
                {
                    camera_pos, camera_dir, camera_up, camera_u, camera_v, camera_w :: Vector3
                }

init_camera_vectors :: Vector3 -> Vector3 -> Vector3 -> CameraVectors
init_camera_vectors pos target up = CameraVectors pos (vnormalise (target - pos)) up u v w
    where w = vnormalise (pos - target)
          u = vnormalise (vcross up w)
          v = vnormalise (vcross w u)

unpack_camera_data :: CameraVectors -> (Vector3, Vector3, Vector3, Vector3, Vector3, Vector3)
unpack_camera_data vectors = (pos, dir, up, u, v, w)
        where pos = camera_pos vectors
              dir = camera_dir vectors
              up = camera_up vectors
              (u, v, w) = (camera_u vectors, camera_v vectors, camera_w vectors)

{-# INLINE unpack_camera_data #-}

shift_ray :: Ray -> Vector3 -> ViewPlane -> Ray  -- ray, camera direction and view plane. New ray origin is on view plane
shift_ray (Ray pos ray_dir) dir vp = Ray new_pos ray_dir
    where shift = (1.0 /(vdot ray_dir dir)) * (vp_distance vp)
          new_pos = pos + (shift *| ray_dir)

trace_pixel :: Tracer -> Integer -> [Ray] -> Color  -- returns average color from traced rays
trace_pixel tracer depth rays = (foldr (+) (Color 0 0 0) (map (trace tracer 0) rays)) /. (fromIntegral (length rays))

init_square_samples :: (Double, Double) -> (Double, Double) -> [Vector3] -> [Vector3]  -- initializing pixel samples on view plane
init_square_samples (w, h) (pos_x, pos_y) samples =
        [Vector3 ((x + pos_x) / hw) ((y + pos_y) / hh) 0 | (Vector3 x y _) <- samples]
    where
        (hw, hh) = (w / 2, h / 2)

init_ray_direction :: Vector3 -> CameraVectors -> ViewPlane -> (Double, Double) -> Vector3  -- initializing ray direction by view plane coordinates
init_ray_direction pos vectors vp (x, y) = vnormalise ((x *| u) + (y *| v) - (vp_distance vp *| w))
    where
        (u, v, w) = (camera_u vectors, camera_v vectors, camera_w vectors)

data PinholeCamera = PinholeCamera  -- perspective projection camera
                    {
                        pinhole_camera_data :: CameraVectors,
                        view_plane :: ViewPlane
                    }

instance Camera PinholeCamera where
    init_pixel camera x y =
            map (\x -> shift_ray x (camera_dir vectors) (view_plane camera)) rays
        where vp = view_plane camera
              vectors = pinhole_camera_data camera
              (pos, dir, _, u, v, w) = unpack_camera_data (pinhole_camera_data camera)
              (hw, hh) = (fromIntegral (width vp) / 2, (fromIntegral (height vp)) / 2)
              (_x, _y) = ((fromIntegral x) - hw, (fromIntegral y) - hh)
              points = init_square_samples (fromIntegral (width vp), fromIntegral (height vp)) (_x, _y) (square_samples (sampler vp))
              dirs = map (init_ray_direction (camera_pos vectors) vectors vp) [(x, y) | Vector3 x y _ <- points]
              rays = map (Ray pos) dirs


    render_scene camera tracer = [[ trace_pixel tracer 0 (init_pixel camera x y)
                                    | x <- [0..(width (view_plane camera)) - 1]]
                                    | y <- [0..(height (view_plane camera))  - 1]]


data DOFCamera = DOFCamera
                    {
                        dof_camera_data :: CameraVectors,
                        dof_view_plane :: ViewPlane,
                        radius, focus_distance :: Double
                    }

instance Camera DOFCamera where
    init_pixel camera x y = []
    render_scene camera tracer = [[]]

