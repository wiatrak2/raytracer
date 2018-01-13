module Color = 
struct
	type t = {
		r : float;
		g : float;
		b : float;
	}

	let make (r: float) (g: float) (b: float) = { r = r; g = g; b = b }
	let get color = (color.r, color.g, color.b)

	let randColor () = {
		r = Random.float 1.;
		g = Random.float 1.;
		b = Random.float 1.;
	}
end

let zero = Color.make 0. 0. 0.

module Material =
struct
	type t = {
		color: Color.t;
		ambient: float;
		lambert: float;
		specular: float;
	}

	let make (color: Color.t) (ambient: float) (lambert: float) (specular: float) = 
	{
		color = color;
		ambient = ambient;
		lambert = lambert;
		specular = specular;
	}

	let randMaterial () =
	{
		color = Color.randColor();
		ambient = Random.float 1.;
		lambert = Random.float 1.;
		specular = Random.float 1.;
	}
	
	let get material = material.color
end