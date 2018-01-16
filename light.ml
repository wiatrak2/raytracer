open Vec
open Ray
open Material
open Object
open World
open Intersection
open Trace
open LightSource

module Light = 
struct

  let distSqToPoint (point1: Vec3f.vec3) (point2: Vec3f.vec3) =
    let subPoints = Vec3f.sub point1 point2 in
    Vec3f.dot subPoints subPoints

	let isVisible (light: lightSource) (point: Vec3f.vec3) (world: World.t) =
		let photonRay = Ray.make point (LightSource.getSource light) in
		let (lightVisibility, _) = Trace.getIntersection photonRay world in
		match lightVisibility with 
			| None -> true
      | Some intersection -> 
        match light with
        | UniformLight_(uLight) -> false
        | _ ->
            let intersectionPoint = Intersection.getPoint intersection in
            let distSqToIntersection = distSqToPoint point intersectionPoint in
            let distSqToLight = LightSource.distanceSqToLight light point in
            if distSqToIntersection < distSqToLight then false else true

	let lambertShading (singleObject: objectType) (point: Vec3f.vec3) (world: World.t) =
		let rec singleLightLambert lights acc =
			match lights with
				| [] -> acc
				| light::tl ->
					match isVisible light point world with
						| false -> singleLightLambert tl acc
						| true  -> 
							let lightDir = LightSource.lightDir light point in
              let coefficient = Vec3f.dot lightDir @@ Object.getNormal singleObject point in
              let distSq = LightSource.distanceSqToLight light point in
              let normal = Object.getNormal singleObject point in
              let lightIntensity = (LightSource.getIntensity light normal) /. distSq in
              if coefficient > 0. then singleLightLambert tl (acc +. (coefficient *. lightIntensity))
							else singleLightLambert tl acc
		in singleLightLambert (World.getLights world) 0.
		
	let phongShading (singleObject: objectType) (point: Vec3f.vec3) (ray: Ray.t) ?(n: float = 10.) (world: World.t) =
		let rec singleLightPhong lights acc =
			match lights with
				| [] -> acc
				| light::tl ->
					if not (isVisible light point world) then singleLightPhong tl acc else
					let normal = Object.getNormal singleObject point in
					let lightDir = LightSource.lightDir light point in
					let v = Vec3f.norm @@ Vec3f.sub (Ray.origin ray) point in
					let r = Vec3f.reflectVec lightDir normal in
					let vDotR = Vec3f.dot v r in
          let phong = max 0. vDotR in
          let distSq = LightSource.distanceSqToLight light point in
          let lightIntensity = (LightSource.getIntensity light normal) /. distSq in    
					singleLightPhong tl @@ acc +. (lightIntensity *. phong ** n)
		in singleLightPhong (World.getLights world) 0.
	
	let rec getShadedColor (singleObject: objectType) (point: Vec3f.vec3) (ray: Ray.t) (reflectDepth: int) (world: World.t) =
		let objectMaterial = Object.getMaterial singleObject in
		let objectColor = Material.get objectMaterial in
		let objectColorVec = Color.colorToVec objectColor in
		let lambertCoefficient = lambertShading singleObject point world in
		let lambertCoefficient = min lambertCoefficient 1. in
		let specCoefficient = phongShading singleObject point ray world in 
		let(ambient, mirror, lambert, specular) = Material.getCoefficients objectMaterial in
		let specVec = Vec3f.smul (specCoefficient *. specular) objectColorVec in
		let ambientVec = Vec3f.smul ambient objectColorVec in

		let mirrorColor = mirrorReflection singleObject point ray reflectDepth world in
		let mirrorColorVec = Color.colorToVec mirrorColor in 
		let mirrorColorCoeff = Vec3f.smul mirror mirrorColorVec in

		let lambertVec = Vec3f.smul (lambertCoefficient *. lambert) objectColorVec in
		let finalColorVec = Vec3f.add specVec @@ Vec3f.add ambientVec @@ Vec3f.add mirrorColorCoeff lambertVec in
		let (x, y, z) = Vec3f.get finalColorVec in
		let x, y, z = min 1. x, min 1. y, min 1. z in
		Color.make x y z
		
	and mirrorReflection (singleObject: objectType) (point: Vec3f.vec3) (ray: Ray.t) (reflectDepth: int) (world: World.t) = 
		let v = Vec3f.norm @@ Vec3f.sub (Ray.origin ray) point in
		let normal = Object.getNormal singleObject point in
		let reflectedRayDir = Vec3f.reflectVec v normal in 
		let reflectedRay = Ray.make point reflectedRayDir in
		let reflectedColor = Trace.traceRay reflectedRay world in
		match reflectedColor with
			| (bgColor, None, _) -> bgColor
			| (objectColor, Some(intersected), intersectPoint)  -> 
				if reflectDepth > 0 then 
					getShadedColor intersected intersectPoint reflectedRay (reflectDepth - 1) world
				else 
					objectColor
		
end