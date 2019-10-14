let () =
  Alcotest.run "ocaml-pitch test"
    [ ("Byte stream reader", Byte_stream_reader_test.tests)
    ; ("Fourier", Fourier_test.tests)
    ; ("Pitch", Pitch_test.tests) ]
