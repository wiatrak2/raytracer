open Vec
open Ray
open Sphere
open Object
open Intersection
open Material
open World

module Trace = 
struct
  (*
    for given ray and world check, if the ray intersects with any of world's object
    if so return Intersection object of the nearest intersection and Object representing
    the intersected object
  *)
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
							let (currentT, newT) = ((Intersection.getT existingIntersection), (Intersection.getT intersection)) in
							if newT < currentT then begin
								nearestIntersection := intersectionTest;
								intersectedObject := Some(singleObject);
							end
		in List.iter (fun x -> checkIntersection x) (World.getObjects world);
		(!nearestIntersection, !intersectedObject)

  (*
    for given ray and world check if ray intersects with any object.
    if so return color of the object, its representation and point of intersection in space
  *)
	let traceRay (ray: Ray.t) (world: World.t) =
		let (nearestIntersection, intersectedObject) = getIntersection ray world in
		match nearestIntersection with
			| None -> (World.getBackgroundColor world, None, (vec3f 0. 0. 0.))
			| Some intersection -> (Intersection.getColor intersection, intersectedObject, (Intersection.getPoint intersection))
end		