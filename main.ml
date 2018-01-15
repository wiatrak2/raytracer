open Raytracer
open Output
open Input

let main() = 
  let (camera, screen, world, reflectDepth, outputFile, gammaCorrection, brightnessTarget) = Input.getWorld "input.json" in
  Raytracer.trace ~reflectDepth:reflectDepth camera world;
  Output.output outputFile brightnessTarget ~gammaCorrection: gammaCorrection screen

let () = main ()