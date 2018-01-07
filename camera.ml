open Vec
open Ray
open Material

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
		res: int * int;
		aspect_ratio: float;
		pixels: Color.t array;
	}

	let make w h res = 
		let (wr, hr) = res in
		{
			width = w;
			height = h;
			res = res;
			aspect_ratio = w /. h;
			pixels = Array.init (wr * hr) @@ fun x -> zero;
		}

	let getRes screen = screen.res
	let getPixels screen = screen.pixels
	let setPixel screen index color = Array.set screen.pixels index color 
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

	let getRay camera i j = 
		let rayDir = OrtBase.mul camera.ortb @@ Vec3f.make i j (-1. *. camera.dist) in
		Ray.make camera.origin rayDir
end
