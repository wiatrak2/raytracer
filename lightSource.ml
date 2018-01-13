open Vec

module LightSource = 
struct
	type t = {
		source: Vec3f.vec3
	}

	let make (source: Vec3f.vec3) = { source = source }
	let get light = light.source
end