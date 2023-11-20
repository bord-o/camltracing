open Vec3
open Point3
open Image

module Ray = struct
  type t = { origin : Point3.t; dir : Vec3.t }

  let create origin dir = { origin; dir }

  let at t scalar =
    let dir = Point3.of_vec t.dir in
    Point3.add t.origin (Point3.mul dir scalar)

  let ray_color (ray : t) : Pixel.t =
    let open Vec3 in
    let unit_direction = unitize ray.dir in
    let white = Pixel.to_vec { r = 255; g = 255; b = 255 } in
    (*let blue = Pixel.to_vec { r = 127; g = 178; b = 255 } in*)
    let blue = Pixel.to_vec { r = 255; g = 0; b = 0 } in
    let a = 0.5 *. (1.0 +. y unit_direction) in
    let vecColor = add (mul white (1.0 -. a)) (mul blue a) in
    Pixel.of_vec vecColor
  (* linear interpolation*)
end
