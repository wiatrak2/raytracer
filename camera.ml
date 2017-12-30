open Vec
open Ray

module OrtBase = 
struct
	type t = {
		w : Vec3f.vec3;
		u : Vec3f.vec3;
		v : Vec3f.vec3;
	}

	let make camera look_at up = 
		let w = Vec3f.norm @@ Vec3f.sub camera look_at in
		let u = Vec3f.norm @@ Vec3f.cross up w in
		let v = Vec3f.cross w u in
		{ w = w; u = u; v = v; }

	let mul ort vec = 
		let (x, y, z) = Vec3f.get vec in
		let x = Vec3f.smul x ort.u in
		let y = Vec3f.smul y ort.v in
		let z = Vec3f.smul z ort.w in 
		Vec3f.add x @@ Vec3f.add y z
end

module Screen = 
struct 
	type t = {
		width : float;
		height : float;
		left_bottom : Vec3f.vec3;
		res: int * int;
		aspect_ratio: float;
	}

	let make w h lb res = {
		width = w;
		height = h;
		left_bottom = lb;
		res = res;
		aspect_ratio = w /. h;
	}
end

module Camera = 
struct
	type t = {
		origin: Vec3f.vec3;
		screen: Screen.t;
		dist: float;
		ortb: OrtBase.t;
	}

	let make origin screen dist at up = {
		origin = origin;
		screen = screen;
		dist = dist;
		ortb = OrtBase.make origin at up;
	}

	let get_ray camera i j = 
		let ray_dir = OrtBase.mul camera.ortb @@ Vec3f.make i j (-1. *. camera.dist) in
		Ray.make camera.origin ray_dir
end

let v1 = vec3f 0. 1. (-8.);;
let lb = vec3f (-1.) 1. 1.;;
let s = Screen.make 3.0 3.0 lb (256, 256);;
let cam = Camera.make v1 s 1. (vec3f 0. 0. 0.) (vec3f 0. (-1.) 0.);;
Vec3f.printVec @@ Ray.origin @@ Camera.get_ray cam 5. 5.;;
Vec3f.printVec @@ Ray.direction @@ Camera.get_ray cam 5. 5.;;
