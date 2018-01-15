open Camera
open Material

module Output =
struct
  let output (filename: string) (screen: Screen.t) = 
    let outFile = open_out filename in
    let (w,h) = Screen.getRes screen in 
    let pixels = Screen.getPixels screen in
    Printf.fprintf outFile "P3\n%d %d\n %d\n" w h 255;
    for y = 0 to (h - 1) do
      for x = 0 to (w - 1) do
        let index = x + (y * w) in
        let (r,g,b) = Color.get @@ pixels.(index) in
        let r = int_of_float (r *. 255.) in
        let g = int_of_float (g *. 255.) in
        let b = int_of_float (b *. 255.) in
        Printf.fprintf outFile "%d %d %d " r g b;
      done;
      Printf.fprintf outFile "\n"
    done;
    close_out outFile
end