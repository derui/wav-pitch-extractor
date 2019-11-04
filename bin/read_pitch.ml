open Wav_pitch_extractor

let split_array ary num =
  let len_child_array = Array.length ary / num in
  List.init num (fun i -> Array.sub ary (i * len_child_array) len_child_array)

let pitch_to_data_point ~sec ~msec ~pitch = Printf.sprintf "%d.%d\t%f\n" sec msec pitch

let data_to_fixed_hz data =
  let data = Fourier.fft_float @@ Array.map Int32.to_float data |> Array.map Complex.norm in
  let data = Array.sub data 0 (Array.length data / 2) in
  data.(0) <- 0. ; data

let write_data_file ~fname ~origin (t : Wav_type.t) =
  let seconds = Wav_type.seconds t in
  match Wav_data_reader.open_origin (origin, t) with
  | Ok data_reader -> (
      let out = open_out fname in
      let rec loop second =
        if second >= seconds then ()
        else
          let data = Wav_data_reader.read ~sec:second data_reader in
          match data with
          | Error _ -> ()
          | Ok data ->
              let arrays =
                split_array data 10
                |> List.map (fun data ->
                       data_to_fixed_hz data |> fun data -> Pitch.detect_pitch_in ~second:0.1 ~data)
              in
              List.iteri
                (fun i pitch ->
                  output_string out @@ pitch_to_data_point ~sec:second ~msec:i ~pitch)
                arrays ;
              Printf.printf "second: %d/%d %f\n%!" (succ second) seconds (Sys.time ()) ;
              loop (succ second)
      in
      try loop 0 ; close_out out
      with e -> close_out out ; Wav_data_reader.close data_reader ; raise e )
  | Error e -> failwith e

let write_script_file fname data_file =
  let out = open_out fname in
  try
    let pitches =
      Pitch.pitches_with_notation ()
      |> List.map (fun (notation, pitch) -> Printf.sprintf "'%s' %f" notation pitch)
      |> String.concat ","
    in
    let script =
      Printf.sprintf
        {|
set ytics (%s)
plot "%s" using 1:2 with lines
set terminal pngcairo
set out "plot.png"
replot
         |}
        pitches data_file
    in
    output_string out script
  with _ -> close_out out

let () =
  let wav_file = Sys.argv.(1) in
  match Wav_reader.open_origin wav_file with
  | Error e -> failwith e
  | Ok reader -> (
    try
      let data = Wav_reader.read reader in
      match data with
      | Error e -> Printf.eprintf "Error occurred: %s" e
      | Ok v ->
          Printf.printf "WAV info\n%s" Wav_type.Chunk_fmt.(to_string v.chunk_fmt) ;
          let data_file = Filename.(basename wav_file ^ ".txt")
          and script_file = Filename.(basename wav_file ^ ".plot") in
          write_data_file ~fname:data_file ~origin:wav_file v ;
          write_script_file script_file data_file
    with e -> Wav_reader.close reader ; raise e )
