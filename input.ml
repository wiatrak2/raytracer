open Camera
open Vec
open Ray
open Sphere
open Plane
open Intersection
open Material
open Trace
open World
open Light
open Output
open LightSource

let parseSphere json = 
	let open Yojson.Basic.Util in
	try 
		let center = json |> member "center"
						  |> to_list |> List.map to_float 
						  |> Vec3f.listToVec in
		let radius = json |> member "radius" |> to_float in
		let color = json |> member "color"
						 |> to_list |> List.map to_float 
						 |> Vec3f.listToVec in
		let ambient = json |> member "ambient" |> to_float in
		let mirror = json |> member "mirror" |> to_float in
		let lambert = json |> member "lambert" |> to_float in
		let specular = json |> member "specular" |> to_float in
		let mat = Material.make (Color.vecToColor color) ambient ~mirror:mirror lambert specular in
		Sphere.make center radius mat
	with
		| e -> Printf.printf "Could not load sphere\n"; raise e

let parsePlane json = 
	let open Yojson.Basic.Util in
	try 
		let point = json |> member "point"
						  |> to_list |> List.map to_float 
						  |> Vec3f.listToVec in
		let norm = json |> member "norm"
						 |> to_list |> List.map to_float 
						 |> Vec3f.listToVec in
		let color = json |> member "color"
						 |> to_list |> List.map to_float 
						 |> Vec3f.listToVec in
		let ambient = json |> member "ambient" |> to_float in
		let mirror = json |> member "mirror" |> to_float in
		let lambert = json |> member "lambert" |> to_float in
		let specular = json |> member "specular" |> to_float in
		let mat = Material.make (Color.vecToColor color) ambient ~mirror:mirror lambert specular in
		Plane.make point (Vec3f.norm norm) mat
	with
		| e -> Printf.printf "Could not load plane\n"; raise e

let parseCamera json = 
	try
		let open Yojson.Basic.Util in 
		let origin = json |> member "origin" |> to_list 
						  |> List.map to_float |> Vec3f.listToVec in
		let lookAt = json |> member "lookAt" |> to_list 
						  |> List.map to_float |> Vec3f.listToVec in
		let width = json |> member "w" |> to_int in
		let height = json |> member "h" |> to_int in
		let fov = json |> member "fov" |> to_float in
		Camera.make origin lookAt fov (width, height)
	with
		| e -> Printf.printf "Could not load camera\n"; raise e


let parseLight json = 
	try
		let open Yojson.Basic.Util in 
		let center = json |> member "center" |> to_list 
						  |> List.map to_float |> Vec3f.listToVec in
		LightSource.make center
	with
		| e -> Printf.printf "Could not load light\n"; raise e

let parseConstants json =
	try
		let open Yojson.Basic.Util in 
		let reflectDepth = json |> member "reflectDepth" |> to_int in
		let color = json |> member "color"
						 |> to_list |> List.map to_float 
						 |> Vec3f.listToVec in
		let ambient = json |> member "ambient" |> to_float in
		let mirror = json |> member "mirror" |> to_float in
		let lambert = json |> member "lambert" |> to_float in
		let specular = json |> member "specular" |> to_float in
		let mat = Material.make (Color.vecToColor color) ambient ~mirror:mirror lambert specular in
		(mat, reflectDepth)
	with
		| e -> Printf.printf "Could not load world's constants\n"; raise e	

let getWorld file_name = 
	let inputJSON = Yojson.Basic.from_file file_name in
  let open Yojson.Basic.Util in
  let outputFile = inputJSON |> member "output" |> to_string in
	let (camera, screen) = inputJSON |> member "camera" |> parseCamera in
	let spheres = inputJSON |> member "spheres" |> to_list |> List.map parseSphere in
	let planes = inputJSON |> member "planes" |> to_list |> List.map parsePlane in
	let spheres = List.map Object.sphereToObject spheres in
	let planes = List.map Object.planeToObject planes in
	let lights = inputJSON |> member "lights" |> to_list |> List.map parseLight in
	let (matW, reflectDepth) = inputJSON |> member "world" |> parseConstants in
	let world = World.make (spheres @ planes) lights matW in
	(camera, screen, world, reflectDepth, outputFile)