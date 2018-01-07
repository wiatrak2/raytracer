open Camera
open Vec
open Ray
open Sphere
open Intersection
open Material
open World
open Output

let trace camera screen = 
	let (w,h) = Screen.getRes screen in

	for i = 0 to (w - 1)  do
		for j = 0 to (h - 1) do
			let col = Color.randColor in
			Screen.setPixel screen (j + (w*i)) col;

		done
	done

let v1 = vec3f 0. 1. (-8.);;
let s = Screen.make 3.0 3.0 (320, 320);;
let cam = Camera.make v1 s 1. (vec3f 0. 0. 0.) (vec3f 0. (-1.) 0.);;
Vec3f.printVec @@ Ray.origin @@ Camera.getRay cam 5. 5.;;
Vec3f.printVec @@ Ray.direction @@ Camera.getRay cam 5. 5.;;
trace cam s;;
let sphere = Sphere.make (vec3f 1. 1. 1.) 1. (Material.make @@ Color.make 0.1 0.2 0.3);;
Intersection.printHit @@ Sphere.checkIntersection sphere (Camera.getRay cam 5. 5.);;

let is = Intersection.make 1.32 v1 (Material.make @@ Color.randColor) in
Printf.printf "in: %.2f\n" @@ Intersection.getT is;;

let fcol = Array.get (Screen.getPixels s) 0 in
let (r,g,b) = Color.get fcol in
Printf.printf "\n%.2f %.2f %.2f\n" r g b;;

output "out.ppm" s