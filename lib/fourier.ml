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
    if n = 0 then accum else if n <= 2 then succ accum else loop (n / 2) (succ accum)
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

let to_s {Complex.re; im} = Printf.sprintf "{%f, %f}" re im

let fft (data : float array) =
  (* make array that has size to align 2-exponential it *)
  let ( ** ) v e =
    let rec loop ret count = if count = 0 then ret else loop (ret * v) (pred count) in
    loop 1 e
  in
  let data_size = Array.length data in
  let size_fixed_data =
    least_exponent_of_2 data_size |> ( ** ) 2 |> Array.make |> fun init -> init Complex.zero
  in
  Array.iteri (fun i v -> size_fixed_data.(i) <- Complex.{re = v; im = 0.}) data ;
  (* define parameter for FFT *)
  let q = 2 in
  (* let n = Array.length size_fixed_data in
   * let degree = least_exponent_of_2 n in *)
  let rec fft' data n stride =
    if n = 1 then data
    else
      let data_fst =
        fft'
          (Array.init (n / q) (fun i ->
               Printf.printf "first %d:%s\n"
                 (i * q * stride)
                 (to_s size_fixed_data.(i * q * stride)) ;
               data.(i * q * stride)))
          (n / q) stride
      and data_snd =
        fft'
          (Array.init (n / q) (fun i ->
               Printf.printf "second %d:%s\n"
                 (stride + (i * q * stride))
                 (to_s size_fixed_data.(stride + (i * q * stride))) ;
               data.(stride + (i * q * stride))))
          (n / q) stride
      in
      let ret = Array.make n Complex.zero in
      loop 0 (n / 2) ~f:(fun i ->
          let omega =
            { Complex.re = cos (-2. *. Float.pi *. float_of_int i /. float_of_int n)
            ; im = sin (-2. *. Float.pi *. float_of_int i /. float_of_int n) }
          in
          let t = data_fst.(i) and t2 = data_snd.(i) in
          ret.(i) <- Complex.add t (Complex.mul t2 omega) ;
          ret.(i + (n / 2)) <- Complex.sub t (Complex.mul t2 omega) ;
          Printf.printf "omega:%s %d:%s %d:%s\n" (to_s omega) i
            (to_s data_fst.(i))
            (i + (n / 2))
            (to_s data_snd.(i))) ;
      ret
  in
  let result = fft' size_fixed_data (Array.length size_fixed_data) 1 in
  Array.sub result 0 data_size

(*   let result = Array.make (Array.length size_fixed_data) Complex.zero in
 * loop 0 degree ~f:(fun deg ->
 *     let pdeg = q ** (degree - (deg + 1)) in
 *     (\* 0 <= p < q ^ (k - 1) *\)
 *     loop 0 pdeg ~f:(fun j0 ->
 *         (\* 0 <= p < q ^ (k - 1) *\)
 *         loop 0 (q ** deg) ~f:(fun j1 ->
 *             let idx = j0 + (j1 * pdeg * q) in
 *             (\* 2-exponent fourier transform. That called butterfly calculation *\)
 *             Printf.printf "j0:j1 %d:%d, index %d, deg %d, total degree %d, pdeg %d, next %d\n%!"
 *               j0 j1 idx deg degree pdeg (idx + pdeg) ;
 *             (\* r = 0 *\)
 *             let w1 = Complex.add size_fixed_data.(idx) size_fixed_data.(idx + pdeg)
 *             (\* r = 1 *\)
 *             and w3 =
 *               Complex.add size_fixed_data.(idx) @@ Complex.neg size_fixed_data.(idx + pdeg)
 *             in
 *             let angle = 2. *. Float.pi *. float_of_int pdeg in
 *             (\* r = 0 *\)
 *             let rotate_factor1 = {Complex.re = 1.; im = 0.}
 *             (\* r = 1 *\)
 *             and rotate_factor2 =
 *               Complex.
 *                 {re = cos (angle *. float_of_int idx); im = sin (angle *. float_of_int idx)}
 *             in
 *             (\* r = 0 *\)
 *             size_fixed_data.(idx) <- Complex.(mul w1 rotate_factor1) ;
 *             (\* r = 1 *\)
 *             size_fixed_data.(idx + pdeg) <- Complex.(mul w3 rotate_factor2)))) ;
 * (\* inverse bit *\)
 * loop 0 (Array.length size_fixed_data) ~f:(fun i ->
 *     let idx = ref i in
 *     let idx_buf = ref 0 in
 *     loop 0 degree ~f:(fun deg ->
 *         let deg = degree - succ deg in
 *         idx_buf := !idx_buf + ((!idx - (!idx / q * q)) * (q ** deg)) ;
 *         idx := !idx / q) ;
 *     result.(i) <- size_fixed_data.(!idx_buf)) ;
 * Array.sub result 0 data_size *)
