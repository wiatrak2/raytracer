open Vec
open Ray
open Material
open Object
open World
open Trace
open LightSource

module Light = 
struct

	let isVisible (light: LightSource.t) (point: Vec3f.vec3) (world: World.t) =
		let photonRay = Ray.make point (LightSource.get light) in
		let (lightVisibility, _) = Trace.getIntersection photonRay world in
		match lightVisibility with 
			| None -> true
			| _ -> false

	let lambertShading (singleObject: objectType) (point: Vec3f.vec3) (lights: LightSource.t list) (world: World.t) =
		let rec singleLightLambert lights acc =
			match lights with
				| [] -> acc
				| light::tl ->
					match isVisible light point world with
						| false -> singleLightLambert tl acc
						| true  -> 
							let lightDir = Vec3f.sub (LightSource.get light) point in
							let unitLightDir = Vec3f.norm lightDir in
							let coefficient = Vec3f.dot unitLightDir @@ Object.getNormal singleObject point in
							if coefficient > 0. then singleLightLambert tl (acc +. coefficient)
							else singleLightLambert tl acc
		in singleLightLambert lights 0.
		
	let phongShading (singleObject: objectType) (point: Vec3f.vec3) (ray: Ray.t) (light: LightSource.t) ?(n: float = 10.) (world: World.t) =
		if not (isVisible light point world) then 0. else
		let normal = Object.getNormal singleObject point in
		let lightDir = Vec3f.sub (LightSource.get light) point in
		let unitLightDir = Vec3f.norm lightDir in
		let v = Vec3f.norm @@ Vec3f.sub (Ray.origin ray) point in
		let r = Vec3f.reflectVec unitLightDir normal in
		let vDotR = Vec3f.dot v r in
		let phong = max 0. vDotR in
		phong ** n
	
	let getShadedColor (singleObject: objectType) (point: Vec3f.vec3) (ray: Ray.t) (lights: LightSource.t list) (world: World.t) =
		let objectMaterial = Object.getMaterial singleObject in
		let objectColor = Material.get objectMaterial in
		let (x, y, z) = Color.get objectColor in
		let objectColorVec = vec3f x y z in
		let lambertCoefficient = lambertShading singleObject point lights world in
		let lambertCoefficient = min lambertCoefficient 1. in
		let specCoefficient = phongShading singleObject point ray (List.hd lights) world in 
		let specVec = Vec3f.smul (specCoefficient *. objectMaterial.specular) objectColorVec in
		let ambientVec = Vec3f.smul objectMaterial.ambient objectColorVec in
		let lambertVec = Vec3f.smul (lambertCoefficient *. objectMaterial.lambert) objectColorVec in
		let finalColorVec = Vec3f.add specVec @@ Vec3f.add ambientVec lambertVec in
		let (x, y, z) = Vec3f.get finalColorVec in
		let x, y, z = min 1. x, min 1. y, min 1. z in
		Color.make x y z
end