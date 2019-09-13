module R = Ocaml_pitch.Byte_stream_reader

let stream () = Stream.of_bytes @@ Bytes.of_string "\x00\x01\x02\x03"
let string_stream () = Stream.of_bytes @@ Bytes.of_string "abcd"

let tests =
  [ Alcotest.test_case "should read a byte from stream" `Quick (fun () ->
        let stream = stream () in
        let data = R.read_byte stream in
        Alcotest.(check @@ option @@ char) "byte" (Some '\x00') data)
  ; Alcotest.test_case "should read bytes that is word size and big endian" `Quick (fun () ->
        let stream = stream () in
        let data = R.read_int64 ~bytes:2 ~endian:`Big stream in
        Alcotest.(check @@ option @@ int64) "byte" (Some Int64.(of_int 1)) data)
  ; Alcotest.test_case "should read bytes that is word size and little endian" `Quick (fun () ->
        let stream = stream () in
        let data = R.read_int64 ~bytes:2 ~endian:`Little stream in
        Alcotest.(check @@ option @@ int64) "byte" (Some Int64.(of_int 256)) data)
  ; Alcotest.test_case "should read bytes that has three bytes and big endian" `Quick (fun () ->
        let stream = stream () in
        let data = R.read_int64 ~bytes:3 ~endian:`Big stream in
        Alcotest.(check @@ option @@ int64) "byte" (Some Int64.(of_int 258)) data)
  ; Alcotest.test_case "should read bytes that has three bytes and little endian" `Quick (fun () ->
        let stream = stream () in
        let data = R.read_int64 ~bytes:3 ~endian:`Little stream in
        Alcotest.(check @@ option @@ int64) "byte" (Some Int64.(of_int (256 + (2 lsl 16)))) data)
  ; Alcotest.test_case "should read string as big endian from stream" `Quick (fun () ->
        let stream = string_stream () in
        let data = R.read_string ~bytes:4 ~endian:`Big stream in
        Alcotest.(check @@ option @@ string) "bytes" (Some "abcd") data)
  ; Alcotest.test_case "should read string as little endian from stream" `Quick (fun () ->
        let stream = string_stream () in
        let data = R.read_string ~bytes:4 ~endian:`Little stream in
        Alcotest.(check @@ option @@ string) "bytes" (Some "dcba") data) ]
