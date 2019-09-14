type endian =
  [ `Little
  | `Big ]

let read_byte stream =
  try
    let byte = Stream.next stream in
    Some byte
  with Stream.Failure -> None

let bytes_to_int64 bytes =
  let rec bytes_to_int64' accum bytes =
    match bytes with
    | [] -> accum
    | v :: bytes -> bytes_to_int64' Int64.(shift_left accum 8 |> logor (of_int v)) bytes
  in
  bytes_to_int64' Int64.zero bytes

let read_bytes ~bytes ~endian stream =
  if bytes < 0 || bytes > 8 then None
  else
    let rec read_bytes' accum count =
      if count = 0 then List.rev accum
      else
        match read_byte stream with
        | None -> accum
        | Some v -> read_bytes' (v :: accum) (pred count)
    in
    let fetched_bytes = read_bytes' [] bytes in
    if List.length fetched_bytes < bytes then None
    else
      let fetched_bytes =
        match endian with `Little -> List.rev fetched_bytes | `Big -> fetched_bytes
      in
      Some fetched_bytes

let read_int64 ~bytes ~endian stream =
  match read_bytes ~bytes ~endian stream with
  | None -> None
  | Some v -> Some (List.map Char.code v |> bytes_to_int64)

let read_int32 ~bytes ~endian stream =
  match read_int64 ~bytes ~endian stream with None -> None | Some v -> Some (Int64.to_int32 v)

let read_string ~bytes ~endian stream =
  match read_bytes ~bytes ~endian stream with
  | None -> None
  | Some v -> Some String.(List.to_seq v |> of_seq)
