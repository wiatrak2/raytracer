open Vec
open Ray
open Sphere
open Object
open Intersection
open Material
open World

module Trace = 
struct
	let getIntersection (ray: Ray.t) (world: World.t) = 
		
		let nearestIntersection: Intersection.t option ref = ref None in
		let intersectedObject: objectType option ref = ref None in

		let checkIntersection (singleObject: objectType) =
			let intersectionTest = Object.checkIntersection singleObject ray in
			match intersectionTest with
				| None -> ()
				| Some intersection -> 
					match !nearestIntersection with
						| None -> 
							nearestIntersection := intersectionTest;
							intersectedObject := Some(singleObject);
						| Some existingIntersection ->
							let currentT, newT = (Intersection.getT existingIntersection), (Intersection.getT intersection) in
							if newT < currentT then 
								nearestIntersection := intersectionTest;
								intersectedObject := Some(singleObject);
		in List.iter (fun x -> checkIntersection x) (World.getObjects world);
		(!nearestIntersection, !intersectedObject)


	let traceRay (ray: Ray.t) (world: World.t) =
		let (nearestIntersection, intersectedObject) = getIntersection ray world in
		match nearestIntersection with
			| None -> (World.getBackgroundColor world, None, (vec3f 0. 0. 0.))
			| Some intersection -> (Intersection.getColor intersection, intersectedObject, (Intersection.getPoint intersection))
end		