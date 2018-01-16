open Vec
open Ray
open Intersection
open Material

module Sphere = 
struct
  (*
    module representing sphere
    center - point of sphere center in space 
    radius - sphere's radius
    material - sphere's material
  *)
	type t = {
		center : Vec3f.vec3;
		radius : Vec3f.t;
		material  : Material.t;
	}

	let make (center: Vec3f.vec3) (radius: Vec3f.t) (material: Material.t) = {
		center = center;
		radius = radius;
		material  = material;
	}

	let getMaterial sphere = sphere.material

	let getNormal sphere (point: Vec3f.vec3) = 
		let norm = Vec3f.sub point sphere.center in
		Vec3f.norm norm

	let makeRandomSphere (maxRadius: float) (worldCenter: Vec3f.vec3) (worldRadius: float) =
		let radius = Random.float maxRadius in
		let randCenter = Vec3f.randVec worldCenter worldRadius in
		make randCenter radius (Material.randMaterial ())
  
  (*
    for given ray with origin in o and direction d check if there are t1 or t2 such that
    o+t1*d/o+t2*d are ray-sphere intersection points, and make an intersection object for 
    nearer of those points
  *)
	let checkIntersection sphere (ray:Ray.t) = 
		let dist = Vec3f.sub (Ray.origin ray) sphere.center in
		let a = Vec3f.len2 (Ray.direction ray) in
		let b = 2. *. (Vec3f.dot (Ray.direction ray) dist) in
		let c = (Vec3f.len2 dist) -. (sphere.radius *. sphere.radius) in 
		let disc = (b *. b -. 4.0 *. a *. c) in
		let denom = 2. *. a in
			if disc <= 0. then None
			else
				let discSqrt = sqrt disc in
				let t = (-.b -. discSqrt) /. denom in
				if t > Ray.epsilon then Some(Intersection.make t (Ray.pointAt ray t) (Material.get sphere.material))
				else 
					let t = (-.b +. discSqrt) /. denom in
					if t > Ray.epsilon then Some(Intersection.make t (Ray.pointAt ray t) (Material.get sphere.material))
					else None

end

