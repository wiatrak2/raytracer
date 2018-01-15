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

	let make (origin: Vec3f.vec3) (lookAt: Vec3f.vec3) (up: Vec3f.vec3) = 
		let w = Vec3f.norm @@ Vec3f.sub lookAt origin in
		let u = Vec3f.norm @@ Vec3f.cross w up in
		let v = Vec3f.cross u w in
		{ w = w; u = u; v = v; }

	let get ort = (ort.w, ort.u, ort.v)

	let mul ort (vec: Vec3f.vec3) = 
		let (x, y, z) = Vec3f.get vec in
		let x = Vec3f.smul x ort.w in
		let y = Vec3f.smul y ort.u in
		let z = Vec3f.smul z ort.v in 
		Vec3f.add x @@ Vec3f.add y z
end

module Screen = 
struct 
	type t = {
		width : float;
		height : float;
		res: int * int;
		aspectRatio: float;
		pixelW: float;
		pixelH: float;
		pixels: Color.t array;
	}

	let make (w: float) (h: float) (res: int * int) = 
		let (wr, hr) = res in
		{
			width = w;
			height = h;
			res = res;
			aspectRatio = w /. h;
			pixelW = w /. (float_of_int wr);
			pixelH = h /. (float_of_int hr);
			pixels = Array.make (wr * hr) zeroColor;
		}

	let getRes screen = screen.res
	let getSize screen = (screen.width, screen.height)
	let getPixelSize screen = (screen.pixelW, screen.pixelH)
	let getPixels screen = screen.pixels
	let getRatio screen = screen.aspectRatio
	let setPixel screen (index: int) (color: Color.t) = Array.set screen.pixels index color 
end

module Camera = 
struct
	
	type t = {
		origin: Vec3f.vec3;
		screen: Screen.t;
		ortb: OrtBase.t;
	}
	
	let make_ (origin: Vec3f.vec3) (screen: Screen.t) (ortb: OrtBase.t) = {
		origin = origin;
		screen = screen;
		ortb = ortb;
	}

	let getScreen camera = camera.screen
	let getOrigin camera = camera.origin

	let make (origin: Vec3f.vec3) (lookAt: Vec3f.vec3) ?(up: Vec3f.vec3 = vec3f 0. 1. 0.) (fov: float) (res: int * int)  =
    let (width, height) = res in
		let ortb = OrtBase.make origin lookAt up in
		let pi = 3.1415926536 in
		let fovRadian = pi *. (fov /. 2.) /. 180.  in
		let aspectRatio = (float_of_int height) /. (float_of_int width) in
		let halfW = tan fovRadian in
		let halfH = aspectRatio *. halfW in
		let screen = Screen.make (halfW *. 2.) (halfH *. 2.) res in
		let camera = make_ origin screen ortb in
		(camera, screen)
		
	let getRayDir camera (i: int) (j: int) =
		let (i, j) = (float_of_int i, float_of_int j) in
		let (pixelW, pixelH) = Screen.getPixelSize camera.screen in
		let (w, h) = Screen.getSize camera.screen in
		let (halfW, halfH) = (w /. 2., h /. 2.) in
		let scaleW, scaleH = ((i *. pixelW) -. halfW), ((j *. pixelH) -. halfH) in
		let (w, u, v) = OrtBase.get camera.ortb in
		let xcomp, ycomp = Vec3f.smul scaleW u, Vec3f.smul scaleH v in
		Vec3f.norm @@ Vec3f.add (Vec3f.add w xcomp) ycomp

end
