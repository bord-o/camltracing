open Vec3
open Point3
open Image
module F = Format

module Ray = struct
  type t = { origin : Point3.t; dir : Vec3.t }
  type hit_record = { p : Point3.t; normal : Vec3.t; t : float; front : bool }

  let create origin dir = { origin; dir }

  let at t scalar =
    let dir = Point3.of_vec t.dir in
    Point3.add t.origin (Point3.mul dir scalar)

  let ray_color (ray : t) (hr : hit_record option) : Pixel.t =
    let open Vec3 in
    match hr with
    | Some r ->
        (* Color our hit pixel *)
        Pixel.of_vec
        @@ create (1. +. x r.normal, 1. +. y r.normal, 1. +. z r.normal)
    | None ->
        (* generate a backdrop *)
        let unit_direction = unitize ray.dir in
        let white = Pixel.to_vec { r = 255; g = 255; b = 255 } in
        let blue = Pixel.to_vec { r = 127; g = 178; b = 255 } in
        let a = 0.5 *. (1.0 +. y unit_direction) in
        let vecColor = add (mul white (1.0 -. a)) (mul blue a) in
        Pixel.of_vec vecColor

  let color_world ray world =
    let results = List.filter (fun hr -> Option.is_some hr) world in
    if List.length results = 0 then ray_color ray None
    else (
      List.iter
        (fun x ->
          let r = Option.get x in
          Printf.printf "%f  " r.t)
        results;
      print_newline ();
      let sorted =
        List.sort
          (fun l r -> Float.compare (Option.get l).t (Option.get r).t)
          results
      in

      ray_color ray @@ List.hd sorted)

  (* linear interpolation*)
end
