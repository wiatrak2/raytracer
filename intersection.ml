open Vec
open Material

module Intersection = 
struct
	type t = {
		t : float;
		point : Vec3f.vec3;
		material : Material.t;
	}

  let make t point material = { t = t; point = point; material = material}
  let getT intersection = intersection.t
  let getMaterial intersection = intersection.material 
end