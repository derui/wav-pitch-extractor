module F = Wav_pitch_extractor.Fourier

let sin_wave n =
  let radian = Float.pi *. 2. in
  Array.init n (fun v ->
      (sin @@ (10. *. radian *. float_of_int v *. 0.01))
      +. (sin @@ (radian *. 20. *. float_of_int v *. 0.01)))

let single_sin_wave ?(len = 0.) freq n =
  let n' = float_of_int n in
  let radian = Float.pi *. 2. *. float_of_int freq /. n' in
  Array.init n (fun v -> Complex.exp { Complex.re = len; im = radian *. float_of_int v })

let tests =
  [
    Alcotest.test_case "should calculate spectral" `Quick (fun () ->
        let spectral = F.dft @@ single_sin_wave 10 100 in
        Array.iteri (fun i v -> Printf.printf "%d = re: %f, im: %f\n" i v.Complex.re v.im) spectral;
        let spectral = Array.map Complex.norm spectral in
        Alcotest.(check @@ float 0.00001) "data" 100. spectral.(10));
    Alcotest.test_case "should calculate spectral with FFT odd samples" `Quick (fun () ->
        let spectral = sin_wave 101 |> F.fft_float |> Array.map Complex.norm
        and spectral' = sin_wave 101 |> F.dft_float |> Array.map Complex.norm in
        Alcotest.(check @@ array @@ float 0.00001) "FFT with DFT in odd" spectral spectral');
    Alcotest.test_case "should calculate spectral with FFT " `Quick (fun () ->
        let spectral = sin_wave 128 |> F.fft_float |> Array.map Complex.norm
        and spectral' = sin_wave 128 |> F.dft_float |> Array.map Complex.norm in
        Alcotest.(check @@ array @@ float 0.00001) "FFT with DFT" spectral spectral');
    Alcotest.test_case "should calculate via IFFT " `Quick (fun () ->
        let spectral =
          single_sin_wave ~len:1. 10 128 |> F.fft |> F.ifft |> Array.map (fun v -> Complex.norm v)
        and spectral' = single_sin_wave ~len:1. 10 128 |> Array.map (fun v -> Complex.norm v) in
        Alcotest.(check @@ array @@ float 0.00001) "IFFT" spectral' spectral);
  ]
