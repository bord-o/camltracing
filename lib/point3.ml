open Vec3

module type Point3 = sig
  include Vec3

  val of_vec : Vec3.t -> t
  val to_vec : t -> Vec3.t
end

module Point3 : Point3 = struct
  include Vec3

  type t = Vec3.t

  let of_vec (v : Vec3.t) : t = create Vec3.(x v, y v, z v)
  let to_vec (v : t) : Vec3.t = Vec3.create (x v, y v, z v)
end
