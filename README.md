# Haskell ray tracer
Simple ray tracer written in pure Haskell.
Still no lenses are used, for sake of simplicity.

Based on Whitted global illumination model. 
Using Blinn-Phong model instead of Phong model, because calculating reflected ray is a pretty expensive operation.

![alt text](https://raw.githubusercontent.com/kraglik/haskell-ray-tracer/master/result.png)
