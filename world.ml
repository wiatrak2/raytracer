open Vec
open Sphere
open Object
open Intersection
open Ray
open LightSource
open Material

module World = 
struct
	type t = {
		objects: objectType list;
		lights: lightSource list;
		background: Material.t
	}

	let make (objects: objectType list) (lights: lightSource list) (background: Material.t) = {
		objects = objects;
		lights = lights;
		background = background;
	}
	
	let getObjects world = world.objects
	let getLights world = world.lights
	let getBackground world = world.background
	let getBackgroundColor world = Material.get world.background

	let addObject world (singleObject: objectType) = 
		world.objects = singleObject::world.objects
end