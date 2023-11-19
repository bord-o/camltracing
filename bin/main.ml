open Ray
module F =Format


let res_x,res_y = 1600, 900

let bar ~total =
  let open Progress.Line in
  let open Progress.Line.Bar_style in
  let b_style = `Custom (utf8 |> with_color (Progress.Color.ansi `magenta)) in
  let b_width  = `Fixed 80 in  

  list [ spinner (); bar ~width:b_width  ~style:b_style total; count_to total ]

let progress p total = 
    let s = F.sprintf "Rendering pixel %i of %i\n" p total in
    Out_channel.output_string stdout s;
    flush stdout

let render log = 
    let r,g,b = 0, 30, 30 in
    let i = Ray.Image.create res_x res_y (log) (fun x y -> {r=abs @@ (int_of_float @@ sqrt(float_of_int @@ x*x + y*y)) mod 255;g;b}) in
    Ray.Image.to_file "/home/bordo/ray/test/out.ppm" i

let pbar () = 
    Progress.with_reporter (bar ~total:(res_x*res_y)) @@ fun reporter -> 
        render (fun c f -> reporter (1))

let () = pbar ()

