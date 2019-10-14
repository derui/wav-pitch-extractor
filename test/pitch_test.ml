module P = Wav_pitch_extractor.Pitch

let tests =
  [ Alcotest.test_case "should get pitches and notations" `Quick (fun () ->
        let data = P.pitches_with_notation () in
        Alcotest.(check @@ bool) "byte" true List.(mem (P.base_notation, P.base_hz) data))
  ; Alcotest.test_case "should get pitch of the data" `Quick (fun () ->
        let data = [|0.1; 0.; 0.1; 0.5; 0.4|] in
        Alcotest.(check @@ float 0.0001) "pitch" 3. P.(detect_pitch_in ~data ~second:1.)) ]
