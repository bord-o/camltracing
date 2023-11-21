open Rays
open Image
open Vec3
open Point3
open Ray
module F = Format

(* Configuration for viewport *)
(* let aspect_ratio = 16.0 /. 9.0 *)
let aspect_ratio = 16. /. 9.
let image_width = 1600

let image_height =
  let h = int_of_float @@ (float_of_int image_width /. aspect_ratio) in
  if h < 1 then 1 else h

let vp_height = 2.0

(*let vp_width = vp_height *. float_of_int (image_width / image_height) *)
let vp_width =
  vp_height *. float_of_int image_width /. float_of_int image_height

(* Configuration for camera*)
let focal_length = 1.0
let camera_center = Point3.create (0., 0., 0.)

(* vp edge vectors *)
let vp_u = Vec3.create (vp_width, 0., 0.)
let vp_v = Vec3.create (0., -.vp_height, 0.)

(* deltas *)
let p_delta_u = Point3.div (Point3.of_vec vp_u) @@ float_of_int image_width
let p_delta_v = Point3.div (Point3.of_vec vp_v) @@ float_of_int image_height
(* upper left pixel location *)

let vp_upper_left =
  let view_vector = Vec3.create (0., 0., focal_length) in
  let v_camera = Point3.to_vec camera_center in
  let open Vec3 in
  sub (sub (sub v_camera view_vector) (div vp_u 2.)) (div vp_v 2.)

let pixel00_loc =
  Vec3.(
    add vp_upper_left
      (add
         (mul (Point3.to_vec p_delta_u) 0.5)
         (mul (Point3.to_vec p_delta_v) 0.5)))

(* Configuration for progress bar*)
let bar ~total =
  let open Progress.Line in
  let open Progress.Line.Bar_style in
  let b_style = `Custom (utf8 |> with_color (Progress.Color.ansi `magenta)) in
  let b_width = `Expand in

  (*let b_width = `Fixed 80 in *)
  list [ spinner (); bar ~width:b_width ~style:b_style total; count_to total ]

(* run with simple console output*)
let run_smpl () =
  let progress p total =
    (* if p mod 99999 = 0 then (
         let s = F.sprintf "Rendering pixel %i of %i\n" p total in
         Out_channel.output_string stdout s;
         flush stdout)
       else () *)
    ()
  in
  F.printf "Rendering with h: %i, w: %i" image_height image_width;
  let i =
    Image.create image_width image_height progress (fun i j ->
        let open Vec3 in
        let pixel_center =
          let xc, yc = (float_of_int i, float_of_int j) in
          let calc_x = mul (Point3.to_vec p_delta_u) xc in
          let calc_y = mul (Point3.to_vec p_delta_v) yc in
          add pixel00_loc (add calc_x calc_y)
        in

        let ray_direction = sub pixel_center @@ Point3.to_vec camera_center in
        let ray = Ray.create camera_center ray_direction in

        let pixel_color : Pixel.t = Ray.ray_color ray in
        pixel_color)
  in
  Image.to_file "/home/bordo/ray/test/out.ppm" i

(* Run with progress bar *)
let run_pbar () =
  let render pbar =
    F.printf "Rendering with h: %i, w: %i" image_height image_width;
    let i =
      Image.create image_width image_height pbar (fun i j ->
          let open Vec3 in
          let pixel_center =
            let xc, yc = (float_of_int i, float_of_int j) in
            let calc_x = mul (Point3.to_vec p_delta_u) xc in
            let calc_y = mul (Point3.to_vec p_delta_v) yc in
            add pixel00_loc (add calc_x calc_y)
          in

          let ray_direction = sub pixel_center @@ Point3.to_vec camera_center in
          let ray = Ray.create camera_center ray_direction in

          let pixel_color = Ray.ray_color ray in
          pixel_color)
    in
    Image.to_file "/home/bordo/ray/test/out.ppm" i
  in
  Progress.with_reporter (bar ~total:(image_height * image_width))
  @@ fun reporter -> render (fun c f -> reporter 1)

let () = run_smpl ()
