module R = Byte_stream_reader
include Reader_intf

type reading_data =
  { fmt : Wav_type.Chunk_fmt.t option
  ; data : Wav_type.Chunk_data.t option }

module Let_syntax = struct
  let bind v ~f = match v with Error _ as e -> e | Ok v -> f v
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

let read_data_chunk stream sample_size channels =
  let%bind size = chunk_size stream in
  let byte_per_sample = sample_size * channels in
  let rec drop_stream n =
    if n <= Int64.zero then ()
    else (
      Stream.junk stream ;
      drop_stream Int64.(pred n) )
  in
  let initial_position = Stream.count stream in
  drop_stream Int64.(mul size @@ Int64.of_int byte_per_sample) ;
  Ok
    { Wav_type.Chunk_data.total_samples = Int64.(div size @@ of_int (channels * sample_size))
    ; initial_position }

let rec read_chunk reading stream =
  match R.read_string ~bytes:4 ~endian:`Big stream with
  | Some "fmt " ->
      let%bind fmt = read_fmt_chunk stream in
      read_chunk {reading with fmt = Some fmt} stream
  | Some "data" -> (
    match reading.fmt with
    | Some fmt ->
        let bits_per_sample = Wav_type.Chunk_fmt.real_bits_per_sample fmt in
        let%bind data =
          read_data_chunk stream (bits_per_sample / 8) (Wav_type.Channels.to_int fmt.channels)
        in
        read_chunk {reading with data = Some data} stream
    | None -> Error "data chunk must locate after fmt chunk" )
  | Some _ -> (
    match R.read_int64 ~bytes:4 ~endian:`Little stream with
    | Some size ->
        R.read_string ~bytes:Int64.(to_int size) ~endian:`Big stream |> ignore ;
        read_chunk reading stream
    | None -> Error "Can not junk unused chunk" )
  | None -> Ok reading

type t =
  { mutable close : unit -> unit
  ; stream : Reader_type.byte_stream }

type origin = string

let open_origin origin =
  try
    let chan = open_in origin in
    Ok {close = (fun () -> close_in chan); stream = Stream.of_channel chan}
  with e -> Error (Printexc.to_string e)

let read t =
  try
    let%bind () = riff_header t.stream in
    let%bind _ = chunk_size t.stream in
    let%bind () = wave_header t.stream in
    let%bind reading = read_chunk {fmt = None; data = None} t.stream in
    t.close () ;
    match reading with
    | {fmt = None; _} | {data = None; _} -> Error "Illegal WAVE format"
    | {fmt = Some fmt; data = Some data} -> Ok Wav_type.{chunk_fmt = fmt; data}
  with e -> t.close () ; raise e

let close t = t.close ()
