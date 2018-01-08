open Vec
open Sphere
open Intersection
open Ray
open Material

module World = 
struct
	type t = {
		spheres: Sphere.t list;
		background: Material.t
	}

	let make (spheres: Sphere.t list) (background: Material.t) = {
		spheres = spheres;
		background = background;
	}
	
	let getSpheres world = world.spheres
	let getBackground world = world.background

	let addSphere world (sphere: Sphere.t) = 
		world.spheres = sphere::world.spheres

	let createRandWorld (maxSpheres: int) (maxRadius: float) (worldCenter: Vec3f.vec3) (worldRadius: float) =
		let spheresNum = Random.int maxSpheres + 1 in
		let rec aux spheresNum spheres = 
			match spheresNum with
				| 0 -> spheres
				| _ -> 
					let randSphere = Sphere.makeRandomSphere maxRadius worldCenter worldRadius in
					aux (spheresNum - 1) (randSphere::spheres)
		in let spheres = aux spheresNum [] in
		make spheres (Material.make (Color.randColor()))
end