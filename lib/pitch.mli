val base_hz : float
(** [base_hz] is 440hz *)

val base_notation : string
(** [base_notation] is [A4] *)

val detect_pitch_in : data:float array -> frame_size:float -> float
(** [detect_pitch_in ~data ~frame_size] get the pitch of the data in frame size. *)

val pitches_with_notation : unit -> (string * float) list
(** [pitches_with_notation ()] get the list of pitch HZ and notation *)
