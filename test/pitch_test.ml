module P = Wav_pitch_extractor.Pitch

let tests =
  [
    Alcotest.test_case "should get pitches and notations" `Quick (fun () ->
        let data = P.pitches_with_notation () in
        Alcotest.(check @@ bool) "byte" true List.(mem (P.base_notation, P.base_hz) data));
    Alcotest.test_case "should get pitch of the data" `Quick (fun () ->
        let data = [| 1.1; 1.; 0.1; 0.6; 0.4 |] in
        Alcotest.(check @@ float 0.0001) "pitch" 1.66667 P.(detect_pitch_in ~data ~frame_size:5.));
  ]
