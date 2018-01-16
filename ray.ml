open Vec

module Ray =
struct
  (*
    ray representation
    origin - ray origin in space
    direction - direction of ray from origin
  *)
	type t = { 
		origin : Vec3f.vec3; 
		direction : Vec3f.vec3 
	}
	
	let make (origin: Vec3f.vec3) (direction: Vec3f.vec3) = {
		origin = origin;
		direction = direction
	}
	let origin r = r.origin
	let direction r = r.direction
	let epsilon = 0.0001

	let pointAt ray (t:float) = Vec3f.add ray.origin (Vec3f.smul t ray.direction)
end

