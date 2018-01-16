open Vec
open Sphere
open Object
open Intersection
open Ray
open LightSource
open Material

module World = 
struct
  (*
    module representing world/scene
    objects - objects that exist in our world
    lights - list of light sources
    background - color of world / color in infinity
  *)
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
    
    let createRandWorld (maxObjects: int) (maxRadius: float) ?(lightsNum: int = 1) (worldCenter: Vec3f.vec3) (worldRadius: float) =
      let objectsNum = Random.int maxObjects + 1 in
      let rec makeRandLights lightsNum lights =
        match lightsNum with
          | 0 -> lights
          | _ ->
            let randLightPoint = Vec3f.randVec worldCenter worldRadius in
            let randPointLight = PointLight.make randLightPoint (Random.float 500.) in
            let randPointLight = PointLight_(randPointLight) in
            makeRandLights (lightsNum - 1) (randPointLight::lights)
      in let lights = makeRandLights lightsNum [] in
      let rec aux objectsNum objects = 
    		match objectsNum with
    			| 0 -> objects
    			| _ -> 
    			let randObject = Object.makeRandomObject maxRadius worldCenter worldRadius in
    			aux (objectsNum - 1) (randObject::objects)
    	in let objects = aux objectsNum [] in
    	make objects lights (Material.randMaterial())
end