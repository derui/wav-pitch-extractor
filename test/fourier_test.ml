module F = Ocaml_pitch.Fourier

let sin_wave =
  let radian = Float.pi *. 2. in
  Array.init 100 (fun v ->
      (sin @@ (10. *. radian *. float_of_int v *. 0.01))
      +. (sin @@ (radian *. 20. *. float_of_int v *. 0.01)))

let tests =
  [ Alcotest.test_case "should calculate spectral" `Quick (fun () ->
        let spectral = F.fourier sin_wave in
        let spectral = Array.map Complex.norm spectral in
        Alcotest.(check @@ float 0.00001) "data" spectral.(20) 50.)
  ; Alcotest.test_case "should calculate spectral with FFT" `Quick (fun () ->
        let spectral = F.fft sin_wave |> Array.map Complex.norm
        and spectral' = F.fourier sin_wave |> Array.map Complex.norm in
        Alcotest.(check @@ array @@ float 0.00001) "FFT" spectral spectral') ]
