open Ocaml_pitch

module File_byte_streamer = struct
  type t = string

  let from t =
    let chan = open_in t in
    Ok (Stream.of_channel chan)
end

module R = Reader.Make (File_byte_streamer)

let write_data_file ~fname (t : Wav_type.t) =
  let out = open_out fname in
  let seconds = Wav_type.Chunk_data.seconds ~fmt:t.chunk_fmt t.data in
  let rec loop second total_samples =
    if second >= seconds then ()
    else
      let data = Wav_type.Chunk_data.get_samples_at_sec ~sec:second ~fmt:t.chunk_fmt t.data in
      match data with
      | None -> ()
      | Some data ->
          Array.iteri
            (fun i data -> output_string out Printf.(sprintf "%d\t%ld\n" (total_samples + i) data))
            data ;
          loop (succ second) (total_samples + Array.length data)
  in
  loop 0 0 ; close_out out

let write_script_file fname data_file =
  let out = open_out fname in
  try
    let script =
      Printf.sprintf
        {|
     plot "%s" using 1:2 with lines
     set terminal pngcairo
     set out "plot.png"
     replot
     |}
        data_file
    in
    output_string out script
  with _ -> close_out out

let () =
  let wav_file = Sys.argv.(1) in
  let data = R.read wav_file in
  match data with
  | Error e -> Printf.eprintf "Error occurred: %s" e
  | Ok v ->
      Printf.printf "WAV info\n%s" Wav_type.Chunk_fmt.(to_string v.chunk_fmt) ;
      let data_file = Filename.(basename wav_file ^ ".txt")
      and script_file = Filename.(basename wav_file ^ ".plot") in
      write_data_file ~fname:data_file v ;
      write_script_file script_file data_file
