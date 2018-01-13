open Vec
open Material

module Intersection = 
struct
	type t = {
		t : float;
		point : Vec3f.vec3;
		color : Color.t;
	}

	let make (t: float) (point: Vec3f.vec3) (color: Color.t)  = { 
		t = t; 
		point = point; 
		color = color;	
	}
	  
  	let getT intersection = intersection.t
	let getColor intersection = intersection.color 

end