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

	let addSphere world (sphere: Sphere.t) = 
		world.spheres = sphere::world.spheres

	let getSpheres world = world.spheres
	let getBackground world = world.background
end