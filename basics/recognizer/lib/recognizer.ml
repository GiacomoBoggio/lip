let lang1 word =
  let rec aux chars =
    match chars with
    | [] -> true
    | '0' :: rest -> aux rest
    | '1' :: rest -> aux rest
    | _ -> false
  in
  aux word

let lang2 word =
  let rec aux seen_zero chars =
    match chars with
    | [] -> true
    | '0' :: rest -> aux true rest
    | '1' :: rest when seen_zero -> aux seen_zero rest
    | _ -> false
  in
  aux false word

let lang3 word =
  let rec aux chars found_zero =
    match chars with
    | [] -> found_zero  (* The word is valid if we found at least one '0' *)
    | '0' :: rest -> aux rest true
    | '1' :: rest when found_zero && List.length rest > 0 && List.hd (List.rev rest) = '0' -> aux [] found_zero
    | _ -> false
  in
  aux word false

let lang4 word =
  let rec aux zeros ones count chars =
    match chars with
    | [] -> count = 2  (* We should have exactly two '1's *)
    | '0' :: rest -> aux (zeros + 1) ones count rest
    | '1' :: rest -> aux zeros (ones + 1) (count + 1) rest
    | _ -> false
  in
  aux 0 0 0 word

let lang5 word =
  let rec aux chars =
    match chars with
    | [] -> true
    | '0' :: rest when rest <> [] && List.hd rest = '0' -> aux (List.tl rest)
    | '1' :: rest when rest <> [] && List.hd rest = '1' -> aux (List.tl rest)
    | _ -> false
  in
  aux word

let recognizers = [lang1; lang2; lang3; lang4; lang5]

let belongsTo w = List.map (fun f -> f w) recognizers
