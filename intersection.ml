open Vec
open Material

module Intersection = 
struct
	type t = {
		intersectionT : float;
		point : Vec3f.vec3;
		color : Color.t;
	}

	let make (t: float) (point: Vec3f.vec3) (color: Color.t)  = { 
		intersectionT = t; 
		point = point; 
		color = color;	
	}
	  
	let getT intersection = intersection.intersectionT
	let getPoint intersection = intersection.point
	let getColor intersection = intersection.color 

end