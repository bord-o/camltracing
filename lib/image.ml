module F = Format

module Pixel = struct
  type t = { r : int; g : int; b : int }

  let to_vec t =
    Vec3.Vec3.create
      ( float_of_int t.r /. 255.,
        float_of_int t.g /. 255.,
        float_of_int t.b /. 255. )

  let of_vec (v : Vec3.Vec3.t) =
    let open Vec3.Vec3 in
    {
      r = int_of_float (x v *. 255.);
      g = int_of_float (y v *. 255.);
      b = int_of_float (z v *. 255.);
    }

  let pp_ppm (p : t) = F.sprintf "%i %i %i " p.r p.g p.b
end

module type Image = sig
  type t

  val create :
    int -> int -> (int -> int -> unit) -> (int -> int -> Pixel.t) -> t

  val show : t -> string
  val to_file : string -> t -> unit
end

module Image : Image = struct
  (*  *)
  type t = string [@@deriving show]

  let create (w : int) (h : int) (logger : int -> int -> unit)
      (f : int -> int -> Pixel.t) =
    let pix_count = w * h in
    let progress = ref 0 in
    let b = Buffer.create 66536 in
    Buffer.add_string b @@ F.sprintf "P3@\n%i %i@\n255\n" w h;

    (* setup the file *)
    for y = 0 to h - 1 do
      for x = 0 to w - 1 do
        logger !progress pix_count;
        let pixel = f x y in
        incr progress;
        Buffer.add_string b (Pixel.pp_ppm pixel)
      done;
      Buffer.add_string b "\n"
    done;
    let s = Buffer.to_bytes b |> String.of_bytes in
    (* print_endline s; *)
    s

  let to_file path img =
    Out_channel.with_open_bin path (fun oc -> output_string oc img)
end
