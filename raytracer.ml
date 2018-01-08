open Camera
open Vec
open Ray
open Sphere
open Intersection
open Material
open World
open Output

let trace (camera: Camera.t) (world: World.t)  = 
	let screen = Camera.getScreen camera in
	let (w,h) = Screen.getRes screen  in
	let origin = Camera.getOrigin camera in
	for i = 0 to (h - 1)  do
		for j = 0 to (w - 1) do
			let rayDir = Camera.getRayDir camera j (h-i) in
			let ray = Ray.make origin rayDir in
			let col = Trace.traceRay ray world in
			Screen.setPixel screen (j + (w*i)) (Material.get col);
		done;
	done;
	output "output.ppm" screen



let origin = vec3f 0. 1.8 10.;;
let lookAt = vec3f 0. 3. 0.;;
let fov = 60.;;
let res = (640, 480);;
let (camera, screen) = Camera.make origin lookAt fov res;;
let sphere1 = Sphere.make (vec3f 0. 3.5 (-3.)) 3. (Material.make @@ (Color.randColor()));;
let sphere2 = Sphere.make (vec3f (-4.) 2. (-1.)) 0.2 (Material.make @@ (Color.randColor()));;
let sphere3 = Sphere.make (vec3f (-4.) 3. (-1.)) 0.1 (Material.make @@ (Color.randColor()));;
let world = World.make [sphere1;sphere2;sphere3] (Material.make @@ (Color.randColor()));;
let randWorld = World.createRandWorld 15 3. (vec3f 0. 2. (-9.2)) 6.;;
trace camera randWorld;;
