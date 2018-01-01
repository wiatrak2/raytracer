open Vec

module Ray =
struct
	type t = { origin : Vec3f.vec3; direction : Vec3f.vec3 }
	
	let make origin direction = { origin = origin ; direction = direction  }
	let origin r = r.origin
	let direction r = r.direction
	let epsilon = 0.001
end

