module BaseTypes where

import Data.Vector.V3
import Data.Vector.Class

{-# LANGUAGE ExistentialQuantification #-}

------------------------------------------------------------------------------------------------------------------------

color_at_hit :: Hit -> Color
color_at_hit hit = color_at (local_hit_pos hit) textureMap
            where textureMap = texture (material hit) :: Texture

{-# INLINE color_at_hit #-}

------------------------------------------------------------------------------------------------------------------------

data Ray = Ray { origin, direction :: Vector3 }

data Hit = Hit
            {
                hit_pos :: Vector3,             -- global intersection coordinates, needed to cast shadows and shade point
                local_hit_pos :: Vector3,       -- local intersection coordinates, needed for texture mapping
                material :: Material,           -- material of intersected surface
                ray :: Ray,                     -- ray that hits an object
                normal :: Vector3               -- normal to a surface at hit position
            }

------------------------------------------------------------------------------------------------------------------------

data Color = Color {red, green, blue :: Double}

instance Num Color where                        -- all color operations are element-wise
    (+) (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 + r2) (g1 + g2) (b1 + b2)
    (-) (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 - r2) (g1 - g2) (b1 - b2)
    (*) (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 * r2) (g1 * g2) (b1 * b2)
    abs (Color r g b) = Color (abs r) (abs g) (abs b)
    signum (Color r g b) = Color (signum r) (signum g) (signum b)
    fromInteger x = Color (fx) (fx) (fx)
        where fx = fromInteger x

(*.) :: Color -> Double -> Color
(*.) (Color r g b) x = Color (r * x) (g * x) (b * x)
(.*) :: Double -> Color -> Color
(.*) x (Color r g b) = Color (r * x) (g * x) (b * x)

(/.) :: Color -> Double -> Color
(/.) (Color r g b) x = Color (r / x) (g / x) (b / x)

------------------------------------------------------------------------------------------------------------------------

class FigureType a where
    normal_at :: a -> Vector3 -> Vector3        -- normal at selected point in global coordinates
    hit :: Ray -> a -> Maybe (Vector3, Vector3) -- tuple of global and local hit coordinates if ray hits object, else Nothing

class LightType a where
    direction_at :: a -> Vector3 -> Vector3     -- light ray direction from light source to selected point
    color :: a -> Color                         -- color of selected light source
    trace_shadow :: a -> Vector3 -> Scene -> [Material]  -- materials of all intersected surfaces between point and light
    force_at :: a -> Vector3 -> Double

class TextureType a where
    color_at :: Vector3 -> a -> Color           -- color at selected local hit point

class Camera a where
    init_pixel :: a -> Integer -> Integer -> [Ray]
    render_scene :: a -> Tracer -> [[Color]]    -- definition of render function means that all side parameters must be contained in camera's data

class SamplerType a where
    disk_samples :: a -> [Vector3]              -- samples at 2d disk with radius = 1.0
    square_samples :: a -> [Vector3]            -- samples at 2d unit square
    hemisphere_samples :: a -> [Vector3]        -- samples at hemisphere with radius = 1.0

class TracerType a where                        -- tracer must handle a scene
    trace :: a -> Integer -> Ray -> Color       -- integer parameter is current depth that must be compared with tracer's max depth before tracing a ray
    cast_shadow :: a -> Vector3 -> LightSource -> Maybe Color        -- shadow ray is a different type of rays, therefore it must be served with a different function
    scene :: a -> Scene

class ShadingModelType a where                  -- type class for local shading models such as Lambert or Phong
    shade :: TracerType b => a -> Hit -> b -> Color -- tuple contains light source and it's force at point

------------------------------------------------------------------------------------------------------------------------
-- some instances that makes it possible to handle list of data objects with types restricted by selected class

data Figure = forall a. FigureType a => Figure a
instance FigureType Figure where
    normal_at (Figure a) point = normal_at a point
    hit ray (Figure a) = hit ray a

data Texture = forall a. TextureType a => Texture a
instance TextureType Texture where
    color_at point (Texture a) = color_at point a

data LightSource = forall a. LightType a => LightSource a
instance LightType LightSource where
    color (LightSource a) = color a
    direction_at (LightSource a) point = direction_at a point
    trace_shadow (LightSource a) point scene = trace_shadow a point scene
    force_at (LightSource a) point = force_at a point

data Tracer = forall a. TracerType a => Tracer a
instance TracerType Tracer where
    trace (Tracer a) depth ray = trace a depth ray
    cast_shadow (Tracer a) point light = cast_shadow a point light
    scene (Tracer a) = scene a

data ShadingModel = forall a. ShadingModelType a => ShadingModel a
instance ShadingModelType ShadingModel where
    shade (ShadingModel a) hit tracer = shade a hit tracer

data Sampler = forall a. SamplerType a => Sampler a
instance SamplerType Sampler where
    disk_samples (Sampler a) = disk_samples a
    square_samples (Sampler a) = square_samples a
    hemisphere_samples (Sampler a) = hemisphere_samples a

------------------------------------------------------------------------------------------------------------------------

data AmbientLight = AmbientLight Color
instance LightType AmbientLight where
    color (AmbientLight light_color) = light_color
    direction_at _ _ = Vector3 0.0 0.0 0.0
    trace_shadow _ _ _ = []
    force_at _ _ = 1.0

------------------------------------------------------------------------------------------------------------------------

data Material = Material
            {
                ka, kd, ks, kr, kt, p, ior :: Double,     -- p for Phong power and ior for 'index of refraction'
                texture :: Texture
            }

data Scene = Scene
            {
                lights :: [LightSource],
                figures :: [(Figure, Material)],
                ambient :: AmbientLight
            }

------------------------------------------------------------------------------------------------------------------------

