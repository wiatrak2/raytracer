open Camera
open Vec
open Ray
open Sphere
open Intersection
open Material
open Trace
open World
open Light
open Output

let trace (camera: Camera.t) (world: World.t)  = 
	let screen = Camera.getScreen camera in
	let (w,h) = Screen.getRes screen  in
	let origin = Camera.getOrigin camera in
	for i = 0 to (h - 1)  do
		for j = 0 to (w - 1) do
			let rayDir = Camera.getRayDir camera j (h-i) in
			let ray = Ray.make origin rayDir in
			let (col, intersected, point) = Trace.traceRay ray world in
				match intersected with
					| None -> Screen.setPixel screen (j + (w*i)) col;
					| Some intersected -> 
						let col = Light.getShadedColor intersected point ray (List.hd @@ World.getLights world) world in
						Screen.setPixel screen (j + (w*i)) col;
		done;
	done;
	output "output.ppm" screen



let origin = vec3f 0. 1.8 10.;;
let lookAt = vec3f 0. 3. 0.;;
let fov = 60.;;
let res = (640, 480);;
let (camera, screen) = Camera.make origin lookAt fov res;;
let randWorld = World.createRandWorld 17 2.5 (vec3f 0. 2.5 (-20.)) 10.;;
trace camera randWorld;;
