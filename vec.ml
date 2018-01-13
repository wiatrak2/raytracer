module type VEC3 =
sig 
	type t
	type vec3
	val make : t -> t -> t -> vec3
	val get : vec3 -> t *  t * t
	val add : vec3 -> vec3 -> vec3
	val sub : vec3 -> vec3 -> vec3
	val mul : vec3 -> vec3 -> vec3
	val div : vec3 -> vec3 -> vec3
	val smul : t -> vec3 -> vec3
	val dot : vec3 -> vec3 -> t
	val abs : vec3 -> t
	val len2 : vec3 -> t
	val norm : vec3 -> vec3
	val cross : vec3 -> vec3 -> vec3
	val printVec : vec3 -> unit
end

module Vec3f = 
struct
	type t = float
	type vec3 = {
		x : float; 
		y : float; 
		z : float;
	}
	let make x y z = { x = x; y = y; z = z }
	let get vec = (vec.x, vec.y, vec.z)
	let add v1 v2 = { x = v1.x +. v2.x ; y = v1.y +. v2.y ; z = v1.z +. v2.z }
	let sub v1 v2 = { x = v1.x -. v2.x ; y = v1.y -. v2.y ; z = v1.z -. v2.z }
	let mul v1 v2 = { x = v1.x *. v2.x ; y = v1.y *. v2.y ; z = v1.z *. v2.z }
	let div v1 v2 = { x = v1.x /. v2.x ; y = v1.y /. v2.y ; z = v1.z /. v2.z }
	let smul s v = { x = s *. v.x ; y = s *. v.y ; z = s *. v.z }
	let dot v1 v2 = v1.x *. v2.x +. v1.y *. v2.y +. v1.z *. v2.z
	let abs v = sqrt @@ dot v v
	let len2 v = dot v v
	let norm v = let r = 1. /. abs v in 
		{ x = r *. v.x ; y = r *. v.y ; z = r *. v.z }
	let cross v1 v2 = {
		x = v1.y *. v2.z -. v1.z *. v2.y;
    y = v1.z *. v2.x -. v1.x *. v2.z;
    z = v1.x *. v2.y -. v1.y *. v2.x;
	}
	let randVec (mean: vec3) (variance: float) =
		let(mX, mY, mZ) = get mean in
		let x = mX +. (Random.float variance *. 2. -. variance) in
		let y = mY +. (Random.float variance *. 2. -. variance) in
		let z = mZ +. (Random.float variance *. 2. -. variance) in
    make x y z
	let reflectVec vec normal =
		let dotProduct = dot vec normal in
		let d = smul dotProduct normal in
		sub (smul 2. d) vec
		
	let printVec v = Printf.printf "%.2f %.2f %.2f\n" v.x v.y v.z
end

let vec3f = Vec3f.make