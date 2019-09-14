open Reader_type

(** The interface to read WAV from some source *)
module type Wav_reader = sig
  type t

  val read : t -> (Wav_type.t, string) result
end

(** The interface to get byte-stream from any type of t *)
module type Byte_streamer = sig
  type t

  val from : t -> (byte_stream, string) result
end
