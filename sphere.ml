open Vec
open Ray

module Color = 
struct
	type t = {
		r : float;
		g : float;
		b : float;
	}

	let make r g b = { r = r; g = g; b = b }
	let get color = (color.r, color.g, color.b)
end

module Sphere = 
struct
	type t = {
		center : Vec3f.vec3;
		radius : Vec3f.t;
		color  : Color.t;
	}

	let make center radius color = {
		center = center;
		radius = radius;
		color  = color;
	}

	let checkHit sphere (ray:Ray.t) = 
		let dist = Vec3f.sub ray.origin sphere.center in
		let a = Vec3f.len2 ray.direction in
		let b = 2. *. (Vec3f.dot ray.direction dist) in
		let c = Vec3f.len2 dist -. sphere.radius *. sphere.radius in 
		let disc = (b *. b -. 4.0 *. a *. c) in
		let denom = 2. *. a in
			if disc <= 0. then None
			else
				let discSqrt = sqrt disc in
				let t = (-.b -. discSqrt) /. denom in
				if t < Ray.epsilon then
				(*TODO*)
end
