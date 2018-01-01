open Camera
open Vec
open Ray
open Sphere

let trace camera screen = 
	let (w,h) = Screen.getRes screen in
	for i = 0 to w do
		for j = 0 to h do
			Printf.printf "%d %d\n" i j
		done
	done

let v1 = vec3f 0. 1. (-8.);;
let s = Screen.make 3.0 3.0 (5, 5);;
let cam = Camera.make v1 s 1. (vec3f 0. 0. 0.) (vec3f 0. (-1.) 0.);;
Vec3f.printVec @@ Ray.origin @@ Camera.getRay cam 5. 5.;;
Vec3f.printVec @@ Ray.direction @@ Camera.getRay cam 5. 5.;;
trace cam s;;

let sphere = Sphere.make (vec3f 1. 1. 1.) 1. (Color.make 0.1 0.2 0.3);;
Vec3f.printVec @@ Sphere.checkHit sphere (Camera.getRay cam 5. 5.);;