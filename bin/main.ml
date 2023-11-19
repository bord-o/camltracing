open Ray

let r,g,b = 0, 30, 30
let i = Ray.Image.create 1600 900 (fun x y -> {r=abs @@ (int_of_float @@ sqrt(float_of_int @@ x*x + y*y)) mod 255;g;b})
let () = Ray.Image.to_file "/home/bordo/ray/test/out.ppm" i
