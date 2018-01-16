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
  let getSource light = Vec3f.smul (-1.) light.direction
  let getIntensity light (norm: Vec3f.vec3) = light.intensity

  let lightDir light (point: Vec3f.vec3) = Vec3f.smul (-1.) light.direction
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
  let getSource light = light.source
  let getIntensity light (norm: Vec3f.vec3) = light.intensity

  let lightDir light (point: Vec3f.vec3) =
    Vec3f.norm @@ Vec3f.sub light.source point

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

  let getSource light = 
    match light with
      | UniformLight_(uLight) -> UniformLight.getSource(uLight)
      | PointLight_(pLight) -> PointLight.getSource(pLight)

  let getIntensity light = 
    match light with
      | UniformLight_(uLight) -> UniformLight.getIntensity(uLight)
      | PointLight_(pLight) -> PointLight.getIntensity(pLight)

  let lightDir light = 
    match light with
      | UniformLight_(uLight) -> UniformLight.lightDir(uLight)
      | PointLight_(pLight) -> PointLight.lightDir(pLight)

  let distanceSqToLight light = 
    match light with
      | UniformLight_(uLight) -> UniformLight.distanceSqToLight(uLight)
      | PointLight_(pLight) -> PointLight.distanceSqToLight(pLight)

  let pointLight light = PointLight_(light)
  let uniformLight light = UniformLight_(light)
end