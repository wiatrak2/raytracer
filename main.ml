open Raytracer
open Output
open Input

let main() = 
  let (camera, screen, world, reflectDepth, outputFile) = Input.getWorld "input.json" in
  Raytracer.trace ~reflectDepth:reflectDepth camera world;
  Output.output outputFile screen

let () = main ()