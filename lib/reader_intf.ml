module type Closeable = sig
  type t

  val close : t -> unit
  (** [close t] close opened something of [t] *)
end

module type Openable = sig
  type t
  type origin

  val open_origin : origin -> (t, string) result
  (** [open_origin origin] open handle from [origin] *)
end

module type Reader = sig
  type t
  type data

  val read : t -> (data, string) result
  (** [read t] read [data] from [t] *)
end

(** The interface to read WAV from some source *)
module type Wav_reader = sig
  include Reader with type data := Wav_type.t
  include Openable with type t := t
  include Closeable with type t := t
end

(** The interface to read data chunk from source *)
module type Wav_data_reader = sig
  type t

  val read :
    sec:int -> ?channel:[`Merged | `Channel of int] -> t -> (Wav_type.Sample.t, string) result
  (** [read t] read [data] from [t] *)

  include Openable with type t := t and type origin = string * Wav_type.t
  include Closeable with type t := t
end
