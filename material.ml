module Color = 
struct
	type t = {
		r : float;
		g : float;
		b : float;
	}

	let make r g b = { r = r; g = g; b = b }
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
	}

	let make color = {
		color = color
	}
	
	let get material = material.color
end