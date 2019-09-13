(** Define types for reader to avoid cyclic dependency *)

type byte_stream = char Stream.t
(** Stream of binary data *)
