open Vec
open Object

module UniformLight = 
struct
  type t = {
    direction: Vec3f.vec3;
    intensity: float;
  }

	let make (direction: Vec3f.vec3) (intensity: float) = {
    direction = Vec3f.norm direction;
    intensity = intensity;
    }
  let get light = light.direction
  let getIntensity light (norm: Vec3f.vec3) = light.intensity

  let distanceSqToLight light (point: Vec3f.vec3) = 1.
end

module PointLight =
struct
	type t = {
    source: Vec3f.vec3;
    intensity: float;
	}

	let make (source: Vec3f.vec3) (intensity: float) = {
    source = source;
    intensity = intensity;
    }
  let get light = light.source
  let getIntensity light (norm: Vec3f.vec3) = light.intensity

  let distanceSqToLight light (point: Vec3f.vec3) = 
    let subPoints = Vec3f.sub point light.source in
    Vec3f.dot subPoints subPoints
end

type lightSource = 
  | UniformLight_ of UniformLight.t
  | PointLight_ of PointLight.t

module LightSource = 
struct

  let make light =
    match light with
      | UniformLight_(uLight) -> UniformLight_(uLight)
      | PointLight_(pLight) -> PointLight_(pLight)

  let get light = 
    match light with
      | UniformLight_(uLight) -> UniformLight.get(uLight)
      | PointLight_(pLight) -> PointLight.get(pLight)

  let getIntensity light = 
    match light with
      | UniformLight_(uLight) -> UniformLight.getIntensity(uLight)
      | PointLight_(pLight) -> PointLight.getIntensity(pLight)

  let distanceSqToLight light = 
    match light with
      | UniformLight_(uLight) -> UniformLight.distanceSqToLight(uLight)
      | PointLight_(pLight) -> PointLight.distanceSqToLight(pLight)

  let pointLight light = PointLight_(light)
  let uniformLight light = UniformLight_(light)
end