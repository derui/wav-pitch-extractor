open Ocaml_pitch

module File_byte_streamer = struct
  type t = string

  let from t =
    let chan = open_in t in
    Ok (Stream.of_channel chan)
end

module R = Reader.Make (File_byte_streamer)

let () =
  let wav_file = Sys.argv.(1) in
  let data = R.read wav_file in
  match data with
  | Error e -> Printf.eprintf "Error occurred: %s" e
  | Ok v -> Printf.printf "WAV info\n%s" Wav_type.Chunk_fmt.(to_string v.chunk_fmt)
