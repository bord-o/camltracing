open Ray
module F = Format

let%expect_test _ = 
  print_endline "it works";
  [%expect {| it works |}]

let%expect_test "image_boilerplate" = 
  let r,g,b = 0,0,0 in
  let i = Ray.Image.create 3 2 (fun _ _ -> ()) (fun x y -> {r;g;b}) in
  F.printf "%s" @@ Image.show i;

  [%expect {| "P3\n3 2\n255\n0 0 0 0 0 0 0 0 0 \n0 0 0 0 0 0 0 0 0 \n" |}]
  

