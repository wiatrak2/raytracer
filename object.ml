open Sphere
open Ray
open Vec

type objectType = Sphere.t

module Object =
struct
	let checkIntersection singleObject (ray:Ray.t) =
	match singleObject with
			| _ -> Sphere.checkIntersection singleObject ray

	let getNormal singleObject (point: Vec3f.vec3) = 
		match singleObject with
			| _ -> Sphere.getNormal singleObject point

	let getMaterial singleObject =
		match singleObject with
			| _ -> Sphere.getMaterial singleObject
end