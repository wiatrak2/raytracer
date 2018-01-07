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

	let printHit hit = 
		match hit with
		| None -> Printf.printf "NO HIT\n"
		| Some hit ->
			let (r,g,b) = Color.get @@ Material.get hit.material in
			Vec3f.printVec hit.point; 
			Printf.printf " %.2f %.2f %.2f %.2f\n" hit.t r g b
end