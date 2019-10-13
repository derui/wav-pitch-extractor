(** Define types for WAV format *)

(** WAV formats *)
module Wav_format = struct
  type t =
    | PCM
    | EXTENSIBLE
    | Unknown of int

  let of_int = function 1 -> PCM | 0xFFFE -> EXTENSIBLE | _ as v -> Unknown v

  let to_string = function
    | PCM -> "PCM"
    | EXTENSIBLE -> "EXTENSIBLE"
    | Unknown v -> Printf.sprintf "Unknown(%d)" v
end

(** Define channel type *)
module Channels = struct
  type t =
    | Monaural
    | Stereo
    | Unknown of int

  let of_int = function 1 -> Monaural | 2 -> Stereo | _ as v -> Unknown v
  let to_int = function Monaural -> 1 | Stereo -> 2 | Unknown v -> v

  let to_string = function
    | Monaural -> "Monaural"
    | Stereo -> "Stereo"
    | Unknown v -> Printf.sprintf "Unknown(%d)" v
end

module Fmt_extension = struct
  type t =
    { sub_format : Wav_format.t
    ; channel_mask : int64
    ; bits_per_sample : int }

  let to_string t =
    Printf.sprintf "{Format: %s, Channel Mask: %Ld, Bits Per Sample: %d}"
      Wav_format.(to_string t.sub_format)
      t.channel_mask t.bits_per_sample
end

module Chunk_fmt = struct
  type t =
    { wav_format : Wav_format.t
    ; channels : Channels.t
    ; sampling_rate : int
    ; byte_rate : int
    ; block_align : int
    ; bits_per_sample : int
    ; extension : Fmt_extension.t option }

  let real_bits_per_sample t =
    match t.extension with None -> t.bits_per_sample | Some ext -> ext.bits_per_sample

  let to_string t =
    let buffer = Buffer.create 1 in
    Printf.sprintf "fmt chunk\n" |> Buffer.add_string buffer ;
    Printf.sprintf "    Format: %s\n" Wav_format.(to_string t.wav_format)
    |> Buffer.add_string buffer ;
    Printf.sprintf "    Channels: %s\n" Channels.(to_string t.channels) |> Buffer.add_string buffer ;
    Printf.sprintf "    Sample Per Sec: %d\n" t.sampling_rate |> Buffer.add_string buffer ;
    Printf.sprintf "    Avg Bytes Per Sec: %d\n" t.byte_rate |> Buffer.add_string buffer ;
    Printf.sprintf "    Block Align: %d\n" t.block_align |> Buffer.add_string buffer ;
    Printf.sprintf "    Bits Per Sample: %d\n" t.bits_per_sample |> Buffer.add_string buffer ;
    ( match t.extension with
    | None -> Printf.sprintf "    Extension: None\n" |> Buffer.add_string buffer
    | Some e ->
        Printf.sprintf "    Extension: %s\n" Fmt_extension.(to_string e)
        |> Buffer.add_string buffer ) ;
    Buffer.to_bytes buffer |> Bytes.to_string
end

module Chunk_data = struct
  type t =
    { total_samples : int64  (** total number of samples in Raw data *)
    ; initial_position : int }
end

module Sample = struct
  type t = int32 array
  (** WAV data *)

  let make size = Array.make size Int32.zero
end

type t =
  { chunk_fmt : Chunk_fmt.t
  ; data : Chunk_data.t }
(** WAV format type. This type has only required value in WAV format *)

let seconds t =
  let sampling_rate = t.chunk_fmt.Chunk_fmt.sampling_rate in
  let current_samples = Int64.to_int t.data.total_samples in
  current_samples / sampling_rate
