open Vec

(* module representing color, with each of rgb values in range 0-1 *)
module Color = 
struct
	type t = {
		r : float;
		g : float;
		b : float;
	}

	let make (r: float) (g: float) (b: float) = { r = r; g = g; b = b }
	let get color = (color.r, color.g, color.b)

	let colorToVec color = Vec3f.make color.r color.g color.b
	let vecToColor(vec: Vec3f.vec3) = 
		let (r, g, b) = Vec3f.get vec in
		make r g b 

	let randColor () = {
		r = Random.float 1.;
		g = Random.float 1.;
		b = Random.float 1.;
	}
end

let zeroColor = Color.make 0. 0. 0.

(* 
  module representing material of object,
  with coefficients needed to compute lambert/phong/ambient/mirror reflection 
*)
module Material =
struct
	type t = {
		color: Color.t;
		ambient: float;
		mirror: float;
		lambert: float;
		specular: float;
	}

	let make (color: Color.t) (ambient: float) ?(mirror: float = 0.) (lambert: float) (specular: float) =
	if ambient = 0. then
	{
		color = color;
		ambient = ambient;
		mirror = mirror;
		lambert = lambert;
		specular = specular;
	}
	else 
	{
		color = color;
		ambient = ambient;
		mirror = 0.;
		lambert = lambert;
		specular = specular;
	}
	
	let randMaterial () =
	{
		color = Color.randColor();
		ambient = Random.float 1.;
		mirror = 0.;
		lambert = Random.float 1.;
		specular = Random.float 1.;
	}
	
	let get material = material.color
	let getCoefficients material = (material.ambient, material.mirror, material.lambert, material.specular)

end