open Vec
open Ray
open Intersection
open Material

module Sphere = 
struct
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

	let makeRandomSphere (maxRadius: float) (worldCenter: Vec3f.vec3) (worldRadius: float) =
		let radius = Random.float maxRadius in
		let (wX, wY, wZ) = Vec3f.get worldCenter in
		let makeRandomCenter () = 
			let x = wX +. (Random.float worldRadius *. 2. -. worldRadius) in
			let y = wY +. (Random.float worldRadius *. 2. -. worldRadius) in
			let z = wZ +. (Random.float worldRadius *. 2. -. worldRadius) in
			vec3f x y z 
		in make (makeRandomCenter()) radius (Material.make (Color.randColor()))

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
				if t > Ray.epsilon then Some(Intersection.make t (Ray.pointAt ray t) sphere.material)
				else 
					let t = (-.b +. discSqrt) /. denom in
					if t > Ray.epsilon then Some(Intersection.make t (Ray.pointAt ray t) sphere.material)
					else None

end

