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

let fft_2_power (data : Complex.t array) =
  let q = 2 in
  let rec fft' data n stride =
    if n = 1 then data
    else
      let data_fst = fft' (Array.init (n / q) (fun i -> data.(i * q * stride))) (n / q) stride
      and data_snd =
        fft' (Array.init (n / q) (fun i -> data.(stride + (i * q * stride)))) (n / q) stride
      in
      let ret = Array.make n Complex.zero in
      loop 0 (n / 2) ~f:(fun i ->
          let omega =
            { Complex.re = cos (-2. *. Float.pi *. float_of_int i /. float_of_int n)
            ; im = sin (-2. *. Float.pi *. float_of_int i /. float_of_int n) }
          in
          let t = data_fst.(i) and t2 = data_snd.(i) in
          ret.(i) <- Complex.add t (Complex.mul t2 omega) ;
          ret.(i + (n / 2)) <- Complex.sub t (Complex.mul t2 omega)) ;
      ret
  in
  fft' data (Array.length data) 1

let ifft_2_power (data : Complex.t array) =
  let data' = Array.map Complex.conj data in
  let data' = fft_2_power data' in
  let coefficient = {Complex.re = 1. /. (float_of_int @@ Array.length data); im = 0.} in
  Array.map (Complex.mul coefficient) data'

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
  let result = fft_2_power size_fixed_data in
  Array.sub result 0 data_size

let fft_bluestein (data : float array) =
  (* make array that has size to align 2-exponential it *)
  let ( ** ) v e =
    let rec loop ret count = if count = 0 then ret else loop (ret * v) (pred count) in
    loop 1 e
  in
  let data_size = Array.length data in
  let padded_size = 2 ** least_exponent_of_2 ((data_size * 2) - 1) in
  let omega i n =
    { Complex.re = cos (Float.pi *. float_of_int i /. float_of_int n)
    ; im = sin (Float.pi *. float_of_int i /. float_of_int n) }
  in
  let a =
    Array.init padded_size
    @@ fun i ->
    if i < data_size then
      let v = {Complex.re = data.(i); im = 0.} in
      let i = omega (-1 * (i ** 2)) data_size in
      Complex.mul v i
    else Complex.zero
  in
  let b =
    Array.init padded_size
    @@ fun i ->
    if i < data_size then omega (i ** 2) data_size
    else if data_size <= i && i <= padded_size - data_size then Complex.zero
    else omega ((data_size - (i - (padded_size - data_size))) ** 2) data_size
  in
  let a' = fft_2_power a and b' = fft_2_power b in
  let r = Array.init padded_size (fun i -> Complex.mul a'.(i) b'.(i)) |> ifft_2_power in
  Array.init data_size (fun i ->
      let omega = b.(i) in
      let r' = r.(i) in
      Complex.mul omega r')
