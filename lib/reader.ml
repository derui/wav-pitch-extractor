module R = Byte_stream_reader
include Reader_intf

type reading_data =
  { fmt : Wav_type.Chunk_fmt.t option
  ; data : Wav_type.Chunk_data.t option }

module Let_syntax = struct
  let return v = Ok v
  let bind v ~f = match v with Error _ as e -> e | Ok v -> f v
  let map v ~f = match v with Error _ as e -> e | Ok v -> Ok (f v)
end

let riff_header stream =
  match R.read_string ~bytes:4 ~endian:`Big stream with
  | Some "RIFF" -> Ok ()
  | Some _ | None -> Error "Do not have RIFF header"

let wave_header stream =
  match R.read_string ~bytes:4 ~endian:`Big stream with
  | Some "WAVE" -> Ok ()
  | Some _ | None -> Error "Do not have WAVE header"

let chunk_size stream =
  match R.read_int64 ~bytes:4 ~endian:`Little stream with
  | Some v -> Ok v
  | None -> Error "Do not read chunk size"

let read_word stream =
  match R.read_int64 ~bytes:2 ~endian:`Little stream with
  | Some v -> Ok v
  | None -> Error "Do not read WORD data from stream"

let read_dword stream =
  match R.read_int64 ~bytes:4 ~endian:`Little stream with
  | Some v -> Ok v
  | None -> Error "Do not read DWORD data from stream"

let fmt_format stream =
  let%bind data = read_word stream in
  Ok (Wav_type.Wav_format.of_int Int64.(to_int data))

let read_fmt_extension stream =
  let%bind bits_per_sample = read_word stream in
  let%bind channel_mask = read_dword stream in
  let%bind sub_format = read_word stream in
  R.read_string ~bytes:14 ~endian:`Big stream |> ignore ;
  Ok
    Wav_type.Fmt_extension.
      { sub_format = Wav_type.Wav_format.of_int @@ Int64.to_int sub_format
      ; channel_mask
      ; bits_per_sample = Int64.to_int bits_per_sample }

let read_fmt_chunk stream =
  let%bind _ = chunk_size stream in
  let%bind wav_format = fmt_format stream in
  let%bind channels = read_word stream in
  let%bind sampling_rate = read_dword stream in
  let%bind byte_rate = read_dword stream in
  let%bind block_align = read_word stream in
  let%bind bits_per_sample = read_word stream in
  let extension =
    match wav_format with
    | Wav_type.Wav_format.EXTENSIBLE ->
        let%bind extension_size = read_word stream in
        if Int64.(extension_size = zero) then
          (* read extension from stream *)
          read_fmt_extension stream
        else Error "not found extension"
    | _ -> Error "Not have extension"
  in
  Ok
    Wav_type.Chunk_fmt.
      { wav_format
      ; channels = Wav_type.Channels.of_int Int64.(to_int channels)
      ; sampling_rate = Int64.to_int sampling_rate
      ; byte_rate = Int64.to_int byte_rate
      ; block_align = Int64.to_int block_align
      ; bits_per_sample = Int64.to_int bits_per_sample
      ; extension = (match extension with Ok v -> Some v | Error _ -> None) }

let read_data_chunk stream sample_size =
  let rec read_data rest_size pos array =
    let open Wav_type in
    if Int64.(rest_size = zero) then array
    else
      match R.read_int32 ~bytes:sample_size ~endian:`Little stream with
      | Some v ->
          read_data
            Int64.(sub rest_size (of_int sample_size))
            (succ pos)
            (Chunk_data.set ~pos ~datum:v array)
      | None -> array
  in
  let%bind size = chunk_size stream in
  let samples = Int64.(div size (of_int sample_size)) in
  let data = Wav_type.Chunk_data.empty Int64.(to_int samples) in
  let data = read_data size 0 data in
  Ok data

let rec read_chunk reading stream =
  match R.read_string ~bytes:4 ~endian:`Big stream with
  | Some "fmt " ->
      let%bind fmt = read_fmt_chunk stream in
      read_chunk {reading with fmt = Some fmt} stream
  | Some "data" -> (
    match reading.fmt with
    | Some fmt ->
        let bits_per_sample = Wav_type.Chunk_fmt.real_bits_per_sample fmt in
        let%bind data = read_data_chunk stream (bits_per_sample / 8) in
        read_chunk {reading with data = Some data} stream
    | None -> Error "data chunk must locate after fmt chunk" )
  | Some _ -> (
    match R.read_int64 ~bytes:4 ~endian:`Little stream with
    | Some size ->
        R.read_string ~bytes:Int64.(to_int size) ~endian:`Big stream |> ignore ;
        read_chunk reading stream
    | None -> Error "Can not junk unused chunk" )
  | None -> Ok reading

module Make (St : Byte_streamer) : Wav_reader with type t = St.t = struct
  type t = St.t

  let read t =
    let%bind stream = St.from t in
    let%bind () = riff_header stream in
    let%bind _ = chunk_size stream in
    let%bind () = wave_header stream in
    let%bind reading = read_chunk {fmt = None; data = None} stream in
    match reading with
    | {fmt = None; _} | {data = None; _} -> Error "Illegal WAVE format"
    | {fmt = Some fmt; data = Some data} -> Ok Wav_type.{chunk_fmt = fmt; data}
end
