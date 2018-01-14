open Camera
open Vec
open Ray
open Sphere
open Plane
open Intersection
open Material
open Trace
open World
open Light
open Output
open LightSource

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
						let col = Light.getShadedColor intersected point ray (World.getLights world) world in
						Screen.setPixel screen (j + (w*i)) col;
		done;
	done;
	output "output.ppm" screen



let origin = vec3f 0. 0. 20.;;
let lookAt = vec3f 0. 0. 0.;;
let fov = 60.;;
let res = (1920, 1080);;
let (camera, screen) = Camera.make origin lookAt fov res;;
(*
let randWorld = World.createRandWorld 10 2.5 (vec3f 0. 2.5 (-20.)) 10.;;
trace camera randWorld;;
*)

(*
let mat1 = Material.make (Color.randColor()) 0.2 0.7 0.2;;
let sphere1 = Sphere.make (vec3f 1. 3.5 (-4.)) 4. mat1;;
let sphere1 = Object.Sphere_(sphere1);;
let mat2 = Material.make (Color.randColor()) 0. 0.9 0.1;;
let sphere2 = Sphere.make (vec3f (-4.) 2. (-1.)) 0.2 mat2;;
let sphere2 = Object.Sphere_(sphere2);;
let mat3 = Material.make (Color.randColor()) 0.1 0.7 0.5;;
let sphere3 = Sphere.make (vec3f (-4.) 3. (-1.)) 0.4 mat3;;
let sphere3 = Object.Sphere_(sphere3);;

let mat4 = Material.make (Color.make 0.67 0.85 1.) 0.3 0.4 0.2;;
let plane1 = Plane.make (vec3f 25. (-3.) (-30.)) (Vec3f.norm(vec3f 0.4 0. 1.)) mat4;;
let plane1 = Object.Plane_(plane1)
*)

let mat1 = Material.make (Color.randColor()) 0.0 0.9 0.;;
let sphere1 = Sphere.make (vec3f (-3.) 0. 4.) 2. mat1;;
let sphere1 = Object.Sphere_(sphere1);;
let mat2 = Material.make (Color.randColor()) 0. 0.9 0.;;
let sphere2 = Sphere.make (vec3f 0. 0. 0.) 2. mat2;;
let sphere2 = Object.Sphere_(sphere2);;
let mat3 = Material.make (Color.randColor()) 0. 0.7 0.;;
let sphere3 = Sphere.make (vec3f 3. 0. 3.) 2. mat3;;
let sphere3 = Object.Sphere_(sphere3);;

let mat4 = Material.make (Color.make 0.67 0.85 1.) 0.0 0.8 0.0;;
let plane1 = Plane.make (vec3f 0. (-2.) 0.) (Vec3f.norm(vec3f 0. 1. 0.)) mat4;;
let plane1 = Object.Plane_(plane1)
let light1 = LightSource.make @@ vec3f (-30.) (10.) 20.;;
let light2 = LightSource.make @@ vec3f 10. 3.5 (-4.)

let matW = Material.make (Color.make 0.5 0.5 0.5) 0. 0.7 0.;;
let world = World.make [sphere1; sphere2; sphere3] [light1] matW;;
trace camera world
