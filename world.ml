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
		lights: LightSource.t list;
		background: Material.t
	}

	let make (objects: objectType list) (lights: LightSource.t list) (background: Material.t) = {
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

	let createRandWorld (maxObjects: int) (maxRadius: float) ?(lightsNum = 1) (worldCenter: Vec3f.vec3) (worldRadius: float) =
		let objectsNum = Random.int maxObjects + 1 in
		let rec makeRandLights lightsNum lights =
			match lightsNum with
				| 0 -> lights
				| _ ->
					let randLightPoint = Vec3f.randVec worldCenter worldRadius in
					let randLight = LightSource.make randLightPoint in
					makeRandLights (lightsNum - 1) (randLight::lights)
		in  let lights = makeRandLights lightsNum [] in
		let rec aux objectsNum objects = 
			match objectsNum with
				| 0 -> objects
				| _ -> 
					let randObject = Object.makeRandomObject maxRadius worldCenter worldRadius in
					aux (objectsNum - 1) (randObject::objects)
		in let objects = aux objectsNum [] in
		make objects lights (Material.randMaterial()) 
end