open Function

let dft (data : Complex.t array) =
  let size = Array.length data in
  let radian = -2. *. Float.pi /. float_of_int size in
  Array.mapi
    (fun index _ ->
      let start = ref Complex.zero in
      Array.iteri
        (fun index' v ->
          let times = float_of_int index *. float_of_int index' in
          let next = Complex.exp { Complex.re = 0.; im = radian *. times } in
          let next = Complex.mul v next in
          start := Complex.add next !start)
        data;
      !start)
    data

let dft_float (data : float array) =
  let size = Array.length data in
  let radian = -2. *. Float.pi /. float_of_int size in
  Array.mapi
    (fun index _ ->
      let start = ref Complex.zero in
      Array.iteri
        (fun index' v ->
          let v = { Complex.re = v; im = 0. } in
          let times = float_of_int index *. float_of_int index' in
          let next = Complex.exp { Complex.re = 0.; im = radian *. times } in
          let next = Complex.mul v next in
          start := Complex.add next !start)
        data;
      !start)
    data

let least_exponent_of_2 n =
  let rec loop n accum = if n <= accum then accum else loop n (accum * 2) in
  loop n 1

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
      loop_range 0 (n / 2) ~f:(fun i ->
          let omega =
            Complex.exp
              { Complex.re = 0.; im = -2. *. Float.pi *. float_of_int i /. float_of_int n }
          in
          let t = data_fst.(i) and t2 = data_snd.(i) in
          ret.(i) <- Complex.add t (Complex.mul t2 omega);
          ret.(i + (n / 2)) <- Complex.sub t (Complex.mul t2 omega));
      ret
  in
  fft' data (Array.length data) 1

let ifft_2_power (data : Complex.t array) =
  let data' = Array.map Complex.conj data in
  let data' = fft_2_power data' in
  let coefficient = 1. /. (float_of_int @@ Array.length data) in
  Array.map
    (fun v ->
      let c = Complex.conj v in
      { Complex.re = c.re *. coefficient; im = c.im *. coefficient })
    data'

let fft_general ~converter data =
  (* make array that has size to align 2-exponential it *)
  let ( ** ) v e =
    let rec loop ret count = if count = 0 then ret else loop (ret * v) (pred count) in
    loop 1 e
  in
  let data_size = Array.length data in
  let padded_size = least_exponent_of_2 ((data_size * 2) - 1) in
  let chirp =
    Array.mapi
      (fun i _ ->
        Complex.exp
          {
            Complex.re = 0.;
            im = -1. *. Float.pi *. float_of_int (i ** 2) /. float_of_int data_size;
          })
      data
  in
  let a =
    Array.init padded_size @@ fun i ->
    if i < data_size then
      let v = converter data.(i) in
      Complex.mul v chirp.(i)
    else Complex.zero
  in
  let b =
    Array.init padded_size @@ fun i ->
    if i < data_size then Complex.conj chirp.(i)
    else if data_size <= i && i <= padded_size - data_size then Complex.zero
    else Complex.conj chirp.(padded_size - i)
  in
  let a' = fft_2_power a and b' = fft_2_power b in
  let r = Array.init padded_size (fun i -> Complex.mul a'.(i) b'.(i)) |> ifft_2_power in
  Array.init data_size (fun i -> Complex.mul r.(i) chirp.(i))

let fft = fft_general ~converter:(fun v -> v)
let fft_float = fft_general ~converter:(fun v -> { Complex.re = v; im = 0. })

let ifft data =
  let coefficient = 1. /. (float_of_int @@ Array.length data) in
  Array.map Complex.conj data |> fft
  |> Array.map (fun v ->
         let c = Complex.conj v in
         { Complex.re = c.re *. coefficient; im = c.im *. coefficient })
