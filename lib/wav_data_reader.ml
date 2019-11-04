module R = Byte_stream_reader

type t =
  { chan : in_channel
  ; close : unit -> unit
  ; wav : Wav_type.t }

type origin = string * Wav_type.t

(* read samples with channel from stream. *)
let read_samples t ~stream =
  let channels = Wav_type.Channels.to_int t.wav.chunk_fmt.channels in
  let samples_per_sec = Int64.of_int (channels * t.wav.chunk_fmt.sampling_rate) in
  let sample_size = t.wav.chunk_fmt.bits_per_sample / 8 in
  let rec read_data rest_size pos array =
    if Int64.(rest_size = zero) then array
    else (
      List.init channels (fun _ -> R.read_int32 ~bytes:sample_size ~endian:`Little stream)
      |> List.iteri (fun c v -> match v with Some v -> array.(c).(pos) <- v | None -> ()) ;
      read_data Int64.(pred rest_size) (succ pos) array )
  in
  let data = Array.init channels (fun _ -> Int64.to_int samples_per_sec |> Wav_type.Sample.make) in
  read_data samples_per_sec 0 data

let get_channel_of_sample ~sample ~channel t =
  let samples = t.wav.chunk_fmt.sampling_rate in
  match channel with
  | `Merged ->
      Array.init samples (fun i ->
          let data =
            Array.fold_left
              (fun accum ary -> Int64.(add accum @@ of_int32 ary.(i)))
              Int64.zero sample
          in
          Int64.(div data @@ of_int @@ Wav_type.Channels.to_int t.wav.chunk_fmt.channels)
          |> Int64.to_int32)
  | `Channel v -> sample.(v)

let open_origin (file, wav) =
  let chan = open_in file in
  let close () = close_in chan in
  Ok {chan; close; wav}

let close {close; _} = close ()

let read ~sec ?(channel : [`Merged | `Channel of int] = `Merged) t =
  let channels = Wav_type.Channels.to_int t.wav.chunk_fmt.channels in
  let data_size = Int64.(mul t.wav.data.total_samples @@ of_int channels) in
  let data_pos = sec * t.wav.chunk_fmt.sampling_rate * channels in
  if data_size <= Int64.of_int data_pos then Error "can not read"
  else (
    seek_in t.chan (t.wav.data.initial_position + data_pos) ;
    let stream = Stream.of_channel t.chan in
    let data = read_samples t ~stream in
    let data = get_channel_of_sample ~channel ~sample:data t in
    Ok data )
