(* This code is in the public domain *)

let get = function None -> assert false | Some v -> v
let utc d t = get @@ Ptime.of_date_time (d, (t, +0000))
let t0 = utc (1998, 12, 31) (23, 59, 59)
let t1 = utc (1999, 01, 01) (00, 00, 00)
let () = assert (Ptime.equal (get @@ Ptime.add_posix_s t0 1.) t1)

let () = assert ((Ptime.diff_posix_s t1 t0) = 1.)

let t2 = utc (1998, 12, 31) (23, 59, 60)
let () = assert (Ptime.equal t1 t2)

let y = 9999 (* hypothetical year were this happens *)
let t0 = utc (y, 06, 30) (23, 59, 58)
let t1 = utc (y, 07, 01) (00, 00, 00)
let () = assert (Ptime.diff_posix_s t1 t0 = 2.)

let t2 = utc (y, 06, 30) (23, 59, 59)
let () = assert (Ptime.equal (get @@ Ptime.add_posix_s t0 1.) t2)
