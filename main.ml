open Raytracer
open Output
open Input

let main() = 
  assert ((Array.length Sys.argv) > 1);
  let input = Array.get Sys.argv 1 in
  let (camera, screen, world, reflectDepth, outputFile, gammaCorrection, brightnessTarget) = Input.getWorld input in
  Raytracer.trace ~reflectDepth:reflectDepth camera world;
  Output.output outputFile brightnessTarget ~gammaCorrection: gammaCorrection screen

let () = main ()