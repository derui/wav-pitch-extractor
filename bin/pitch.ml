let base_hz = 440.
let base_notation = "A4"

let before_base_notations =
  [ "C0"
  ; "C#0"
  ; "D0"
  ; "D#0"
  ; "E0"
  ; "F0"
  ; "F#0"
  ; "G0"
  ; "G#0"
  ; "A0"
  ; "A#0"
  ; "B0"
  ; "C1"
  ; "C#1"
  ; "D1"
  ; "D#1"
  ; "E1"
  ; "F1"
  ; "F#1"
  ; "G1"
  ; "G#1"
  ; "A1"
  ; "A#1"
  ; "B1"
  ; "C2"
  ; "C#2"
  ; "D2"
  ; "D#2"
  ; "E2"
  ; "F2"
  ; "F#2"
  ; "G2"
  ; "G#2"
  ; "A2"
  ; "A#2"
  ; "B2"
  ; "C3"
  ; "C#3"
  ; "D3"
  ; "D#3"
  ; "E3"
  ; "F3"
  ; "F#3"
  ; "G3"
  ; "G#3"
  ; "A3"
  ; "A#3"
  ; "B3"
  ; "C4"
  ; "C#4"
  ; "D4"
  ; "D#4"
  ; "E4"
  ; "F4"
  ; "F#4"
  ; "G4"
  ; "G#4" ]

let after_base_notations =
  [ "A4"
  ; "A#4"
  ; "B4"
  ; "C5"
  ; "C#5"
  ; "D5"
  ; "D#5"
  ; "E5"
  ; "F5"
  ; "F#5"
  ; "G5"
  ; "G#5"
  ; "A5"
  ; "A#5"
  ; "B5"
  ; "C6"
  ; "C#6"
  ; "D6"
  ; "D#6"
  ; "E6"
  ; "F6"
  ; "F#6"
  ; "G6"
  ; "G#6"
  ; "A6"
  ; "A#6"
  ; "B6"
  ; "C7"
  ; "C#7"
  ; "D7"
  ; "D#7"
  ; "E7"
  ; "F7"
  ; "F#7"
  ; "G7"
  ; "G#7"
  ; "A7"
  ; "A#7"
  ; "B7"
  ; "C8"
  ; "C#8"
  ; "D8"
  ; "D#8"
  ; "E8"
  ; "F8"
  ; "F#8"
  ; "G8"
  ; "G#8"
  ; "A8"
  ; "A#8"
  ; "B8"
  ; "C9"
  ; "C#9"
  ; "D9"
  ; "D#9"
  ; "E9"
  ; "F9"
  ; "F#9"
  ; "G9"
  ; "G#9"
  ; "A9"
  ; "A#9"
  ; "B9"
  ; "C10"
  ; "C#10"
  ; "D10"
  ; "D#10"
  ; "E10"
  ; "F10"
  ; "F#10"
  ; "G10"
  ; "G#10"
  ; "A10"
  ; "A#10"
  ; "B10" ]

let calculate_pitch n = base_hz *. (2. ** (float_of_int n /. 12.))

(** [pitches_with_notation] returns tuple having notation and hz *)
let pitches_with_notation () =
  let before =
    List.rev before_base_notations
    |> List.mapi (fun i v -> (v, calculate_pitch (-1 * succ i)))
    |> List.rev
  in
  let after = List.mapi (fun i v -> (v, calculate_pitch @@ succ i)) after_base_notations in
  before @ [(base_notation, base_hz)] @ after

(** [detect_pitch_in ~data ~second] get the pitch of the data in second. *)
let detect_pitch_in ~data ~second =
  let max_hz_index = ref 0 in
  let max_hz = ref 0. in
  Array.iteri
    (fun i v ->
      if v > !max_hz then (
        max_hz_index := i ;
        max_hz := v )
      else ())
    data ;
  Printf.printf "max index %d %f\n" !max_hz_index (float_of_int !max_hz_index /. second) ;
  float_of_int !max_hz_index /. second
