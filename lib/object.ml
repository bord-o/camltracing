open Vec3
open Point3
open Ray

type t = [ `Sphere of Point3.t * float ]

let hit (t : t) (ray : Ray.t) ~ray_tmin ~ray_tmax =
  match t with
  | `Sphere (center, radius) ->
      let open Vec3 in
      let oc = sub (Point3.to_vec ray.origin) (Point3.to_vec center) in
      let a = length_squared ray.dir in
      let half_b = dot oc ray.dir in
      let c = length_squared oc -. (radius *. radius) in
      let disc = (half_b *. half_b) -. (a *. c) in

      let compute_hit_record root : Ray.hit_record =
        let tr = root in
        let p = Ray.at ray tr in
        let outward_normal = Point3.div (Point3.sub p center) radius in
        let front = dot ray.dir (Point3.to_vec outward_normal) < 0. in
        let normal =
          if front then Point3.to_vec outward_normal
          else Point3.to_vec outward_normal
        in

        { p; normal; t = tr; front }
      in

      if disc < 0. then None
      else
        let sqrtd = sqrt disc in
        let root = (-.half_b -. sqrtd) /. a in
        if root <= ray_tmin || ray_tmax <= root then
          let root = (-.half_b +. sqrtd) /. a in
          if root <= ray_tmin || ray_tmax <= root then None
          else Some (compute_hit_record root)
        else Some (compute_hit_record root)
