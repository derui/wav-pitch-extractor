type endian =
  [ `Little
  | `Big ]
(** Endianness to read data from stream *)

val read_byte : Reader_type.byte_stream -> char option
(** [read_byte stream] read a byte from the stream. *)

val read_int32 : bytes:int -> endian:endian -> Reader_type.byte_stream -> int32 option
(** [read_int32 ~bytes ~endian stream] read bytes from the stream. [bytes] must be less than 4 = 32bit. *)

val read_int64 : bytes:int -> endian:endian -> Reader_type.byte_stream -> int64 option
(** [read_int64 ~bytes ~endian stream] read bytes from the stream. [bytes] must be less than 8 = 64bit. *)

val read_string : bytes:int -> endian:endian -> Reader_type.byte_stream -> string option
(** [read_string ~bytes ~endian stream] read bytes from the stream. *)
