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
		let photonRay = Ray.make (LightSource.get light) point in
		let (lightVisibility, _) = Trace.getIntersection photonRay world in
		match lightVisibility with 
			| None -> true
			| _ -> false

	let lambertShading (singleObject: objectType) (point: Vec3f.vec3) (light: LightSource.t) (world: World.t) =
		match isVisible light point world with
			| false -> 0.
			| true  -> 
				let contr = Vec3f.sub (LightSource.get light) point in
				let unitContr = Vec3f.norm contr in
				let contribution = Vec3f.dot unitContr @@ Object.getNormal singleObject point in
				if contribution > 0. then contribution
				else 0. 
	
	let specularShading (singleObject: objectType) (point: Vec3f.vec3) (ray: Ray.t) (light: LightSource.t) (world: World.t) =
		let normal = Object.getNormal singleObject point in
		let reflectRay = Ray.make point @@ Vec3f.reflectVec (Ray.direction ray) normal in
		let worldColor = World.getBackgroundColor world in
		let (reflectColor, _) = Trace.traceRay reflectRay world in
		match reflectColor with 
			| worldColor -> vec3f 0. 0. 0.
			| col -> 
				let (x,y,z) = Color.get col in
				let vec = vec3f x y z in
				let objectMat = Object.getMaterial singleObject in
				Vec3f.smul objectMat.specular vec
	
	let getShadedColor (singleObject: objectType) (point: Vec3f.vec3) (ray: Ray.t) (light: LightSource.t) (world: World.t) =
		let objectMaterial = Object.getMaterial singleObject in
		let objectColor = Material.get objectMaterial in
		let (x, y, z) = Color.get objectColor in
		let objectColorVec = vec3f x y z in
		let lambertAmount = lambertShading singleObject point light world in
		let lambertAmount = min lambertAmount 1. in
		let specVec = specularShading singleObject point ray light world in
		let ambientVec = Vec3f.smul objectMaterial.ambient objectColorVec in
		let lambertVec = Vec3f.smul (lambertAmount *. objectMaterial.lambert) objectColorVec in
		let finalColorVec = Vec3f.add specVec @@ Vec3f.add ambientVec lambertVec in
		let (x, y, z) = Vec3f.get finalColorVec in
		Color.make x y z
end