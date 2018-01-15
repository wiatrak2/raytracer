open Camera
open Vec
open Ray
open Intersection
open Trace
open World
open Light

module Raytracer = 
struct
	let trace ?(reflectDepth: int = 3) (camera: Camera.t) (world: World.t) = 
		let screen = Camera.getScreen camera in
		let (w,h) = Screen.getRes screen  in
		let origin = Camera.getOrigin camera in
		for i = 0 to (h - 1)  do
			for j = 0 to (w - 1) do
				let rayDir = Camera.getRayDir camera j (h-i) in
				let ray = Ray.make origin rayDir in
				let (col, intersected, point) = Trace.traceRay ray world in
					match intersected with
						| None -> Screen.setPixel screen (j + (w*i)) col;
						| Some intersected -> 
							let col = Light.getShadedColor intersected point ray reflectDepth world in
							Screen.setPixel screen (j + (w*i)) col;
			done;
		done;
end
