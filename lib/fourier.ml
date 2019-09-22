let fourier (data : float array) =
  let size = Array.length data in
  let radian = 2. *. Float.pi /. float_of_int size in
  Array.mapi
    (fun index _ ->
      let start = ref Complex.zero in
      Array.iteri
        (fun index' v ->
          let v = {Complex.re = v; im = 0.} in
          let times = float_of_int index *. float_of_int index' in
          let next = {Complex.re = cos (radian *. times); im = sin (radian *. times)} in
          let next = Complex.mul v next in
          start := Complex.add next !start)
        data ;
      !start)
    data

let least_exponent_of_2 n =
  let rec loop n accum =
    if n = 0 then accum else if n < 2 then succ accum else loop (n / 2) (succ accum)
  in
  loop n 0

let loop s e ~f =
  let rec loop' current =
    if current >= e then ()
    else (
      f current ;
      loop' (succ current) )
  in
  loop' s

let fft (data : float array) =
  (* make array that has size to align 2-exponential it *)
  let ( ** ) v e =
    let rec loop ret count = if count = 0 then ret else loop (ret * v) (pred count) in
    loop 1 e
  in
  let data_size = Array.length data in
  let size_fixed_data =
    least_exponent_of_2 data_size |> Array.make |> fun init -> init Complex.zero
  in
  let result = Array.make (Array.length size_fixed_data) Complex.zero in
  Array.iteri (fun i v -> size_fixed_data.(i) <- Complex.{re = v; im = 0.}) data ;
  (* define parameter for FFT *)
  let q = 2 in
  let n = Array.length size_fixed_data in
  let degree = least_exponent_of_2 n in
  loop 1 degree ~f:(fun deg ->
      let pdeg = q ** (degree - deg) in
      loop 0 pdeg ~f:(fun j0 ->
          loop 0
            ((q ** (deg - 1)) - 1)
            ~f:(fun j1 ->
              let idx = j0 + (j1 * q * pdeg) in
              (* 2-exponent fourier transform. That called butterfly calculation *)
              let w1 = size_fixed_data.(idx)
              and w2 = Complex.mul size_fixed_data.(idx + pdeg) {Complex.re = -1.; im = 0.} in
              let radian = 2. *. Float.pi in
              let rotate_factor1 = {Complex.re = 1.; im = 0.}
              and rotate_factor2 =
                Complex.
                  { re = cos (radian *. float_of_int idx /. float_of_int (pdeg * q))
                  ; im = sin (radian *. float_of_int idx /. float_of_int (pdeg * q)) }
              in
              size_fixed_data.(idx) <- Complex.(add w1 w2 |> mul rotate_factor1) ;
              size_fixed_data.(idx + pdeg) <- Complex.(add w1 w2 |> mul rotate_factor2)))) ;
  (* inverse bit *)
  loop 0 (Array.length size_fixed_data) ~f:(fun i ->
      let idx = ref i in
      let idx_buf = ref 0 in
      loop 0 degree ~f:(fun deg ->
          let deg = degree - succ deg in
          idx_buf := !idx_buf + ((!idx_buf - (!idx / q * q)) * (q ** deg)) ;
          idx := !idx / q) ;
      result.(i) <- size_fixed_data.(!idx_buf)) ;
  Array.sub result 0 data_size
