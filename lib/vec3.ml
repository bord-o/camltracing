module type Vec3 = sig
  type t

  val create : float * float * float -> t
  val of_int_triple : int * int * int -> t
  val x : t -> float
  val y : t -> float
  val z : t -> float
  val neg : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> float -> t
  val div : t -> float -> t
  val dot : t -> t -> float
  val length_squared : t -> float
  val length : t -> float
  val cross : t -> t -> t
  val unitize : t -> t
end

module Vec3 : Vec3 = struct
  type t = { x : float; y : float; z : float }

  let create (x, y, z) = { x; y; z }
  let x t = t.x
  let y t = t.y
  let z t = t.z

  let of_int_triple (x, y, z) =
    { x = float_of_int x; y = float_of_int y; z = float_of_int z }

  let neg t = { x = -.t.x; y = -.t.y; z = -.t.z }
  let add l r = { x = l.x +. r.x; y = l.y +. r.y; z = l.z +. r.z }
  let sub l r = { x = l.x -. r.x; y = l.y -. r.y; z = l.z -. r.z }
  let mul t scalar = { x = t.x *. scalar; y = t.y *. scalar; z = t.z *. scalar }
  let div t scalar = { x = t.x /. scalar; y = t.y /. scalar; z = t.z /. scalar }
  let length_squared t = (t.x *. t.x) +. (t.y *. t.y) +. (t.z *. t.z)
  let length t = Float.sqrt (length_squared t)
  let dot l r = (l.x *. r.x) +. (l.y *. r.y) +. (l.z *. r.z)
  let unitize t = div t (length t)

  let cross l r =
    {
      x = (l.y *. r.z) -. (l.z *. r.y);
      y = (l.z *. r.x) -. (l.x *. r.z);
      z = (l.x *. r.y) -. (l.y *. r.x);
    }
end
