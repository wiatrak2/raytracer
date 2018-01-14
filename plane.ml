open Vec
open Ray
open Intersection
open Material

module Plane =
struct
	type t = {
		point: Vec3f.vec3;
		normal: Vec3f.vec3;
		material: Material.t;
	}

	let make (point: Vec3f.vec3) (normal: Vec3f.vec3) (material: Material.t) = {
		point = point;
		normal = normal;
		material = material;
	}

	let getMaterial plane = plane.material
	let getNormal plane = plane.normal
	
	let checkIntersection plane (ray:Ray.t) =
		let rayOrigin = Ray.origin ray in
		let rayDir = Ray.direction ray in
		let denom = Vec3f.dot plane.normal rayDir in
		if (abs_float denom) > Ray.epsilon then 
			let rayVec = Vec3f.sub plane.point rayOrigin in
			let tD = Vec3f.dot rayVec plane.normal in
			let t = tD /. denom in
			if t > Ray.epsilon then Some(Intersection.make t (Ray.pointAt ray t) (Material.get plane.material))
			else None
		else None
end