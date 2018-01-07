open Vec
open Ray
open Sphere
open Intersection
open Material
open World

let getIntersection (ray: Ray.t) (world: World.t) = 
	
	let nearestIntersection: Intersection.t option ref = ref None in

	let checkIntersection (sphere: Sphere.t) =
		let intersectionTest = Sphere.checkIntersection sphere ray in
		match intersectionTest with
			| None -> ()
			| Some intersection -> 
				match !nearestIntersection with
					| None -> nearestIntersection := intersectionTest
					| Some existingIntersection ->
						let currentT, newT = (Intersection.getT existingIntersection), (Intersection.getT intersection) in
						if newT < currentT then nearestIntersection := intersectionTest;
	in List.iter (fun x -> checkIntersection x) (World.getSpheres world);
	!nearestIntersection


let traceRay (ray: Ray.t) (world: World.t) =
	let nearestIntersection = getIntersection ray world in
	match nearestIntersection with
		| None -> World.getBackground world
		| Some intersection -> Intersection.getMaterial intersection
		