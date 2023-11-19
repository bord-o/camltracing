open Ray

let r,g,b = 255,255,0
let i = Ray.Image.create 256 256 (fun x y -> {r;g;b})
let () = Ray.Image.to_file "/home/bordo/ray/test/out.ppm" i
