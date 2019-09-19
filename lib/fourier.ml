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
