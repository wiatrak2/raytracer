open Vec
open Material

module Intersection = 
struct
  (*
    module representing intersection with object. 
    intersectionT - parameter such that o + t * d is intersenction point, 
                    where o is ray origin and d is ray direction
    point - coordinates of intersection
    color - color of hitten object
  *)
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