open Vec3
open Point3
open Image
module F = Format

module Ray = struct
  type t = { origin : Point3.t; dir : Vec3.t }

  let create origin dir = { origin; dir }

  let at t scalar =
    let dir = Point3.of_vec t.dir in
    Point3.add t.origin (Point3.mul dir scalar)

  let hit_sphere (center : Vec3.t) (radius : float) (ray : t) =
    let open Vec3 in
    let oc = sub (Point3.to_vec ray.origin) center in
    let a = dot ray.dir ray.dir in
    let b = 2.0 *. dot oc ray.dir in
    let c = dot oc oc -. (radius *. radius) in
    let disc = (b *. b) -. (4.0 *. a *. c) in

    (* F.printf "%f\n" disc; *)
    match disc with
    | x when x < 0. -> -1.
    | _ -> (-.b -. sqrt disc) /. (2.0 *. a)

  let ray_color (ray : t) : Pixel.t =
    let open Vec3 in
    let t = hit_sphere (Point3.to_vec @@ Point3.create (0., 0., -1.)) 0.5 ray in
    if t > 0. then
      let norm =
        unitize @@ sub (Point3.to_vec (at ray t)) (create (0., 0., -1.))
      in
      Pixel.of_vec @@ create (1. +. x norm, 1. +. y norm, 1. +. z norm)
    else
      let unit_direction = unitize ray.dir in
      let white = Pixel.to_vec { r = 255; g = 255; b = 255 } in
      let blue = Pixel.to_vec { r = 127; g = 178; b = 255 } in
      let a = 0.5 *. (1.0 +. y unit_direction) in
      let vecColor = add (mul white (1.0 -. a)) (mul blue a) in
      Pixel.of_vec vecColor
  (* linear interpolation*)
end
