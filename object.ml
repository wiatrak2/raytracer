open Sphere
open Plane
open Ray
open Vec

type objectType =
  | Sphere_ of Sphere.t
  | Plane_ of Plane.t

let sphereToObject (sphere: Sphere.t) = Sphere_(sphere)
let planeToObject (plane: Plane.t) = Plane_(plane)

module Object =
struct
	let make singleObject =
		match singleObject with
			| Sphere_(sphere) -> Sphere_(sphere)
			| Plane_(plane) -> Plane_(plane)

	let checkIntersection singleObject (ray:Ray.t) =
		match singleObject with
    		| Sphere_(sphere) -> Sphere.checkIntersection sphere ray
      		| Plane_(plane) -> Plane.checkIntersection plane ray

	let getNormal singleObject (point: Vec3f.vec3) = 
		match singleObject with
			| Sphere_(sphere) -> Sphere.getNormal sphere point
      		| Plane_(plane) -> Plane.getNormal plane

	let getMaterial singleObject =
		match singleObject with
			| Sphere_(sphere) -> Sphere.getMaterial sphere
     		| Plane_(plane) -> Plane.getMaterial plane

	let makeRandomObject (maxRadius: float) (worldCenter: Vec3f.vec3) (worldRadius: float) =
		Sphere_(Sphere.makeRandomSphere maxRadius worldCenter worldRadius)

	let sphereToObject (sphere: Sphere.t) = Sphere_(sphere)
	let planeToObject (plane: Plane.t) = Plane_(plane)
end