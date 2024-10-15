open Recognizer

let%test _ = lang1 ['0'; '1'; '0'; '1'] = true
let%test _ = lang1 ['2'; '0'; '1'] = false
let%test _ = lang1 ['1'; '1'; '0'; '0'] = true

let%test _ = lang2 ['0'; '1'; '1'; '1'] = true
let%test _ = lang2 ['1'; '1'; '1'] = true
let%test _ = lang2 ['0'; '1'; '0'] = false

let%test _ = lang3 ['0'; '1'; '1'; '0'] = true
let%test _ = lang3 ['1'; '0'; '0'] = false
let%test _ = lang3 ['0'; '1'; '0'; '1'] = false

let%test _ = lang4 ['0'; '1'; '0'; '1'] = true
let%test _ = lang4 ['1'; '0'; '0'] = false
let%test _ = lang4 ['0'; '0'; '1'; '0'; '1'] = true

let%test _ = lang5 ['0'; '0'; '1'; '1'; '0'; '0'] = true
let%test _ = lang5 ['0'; '1'; '1'; '0'] = false
let%test _ = lang5 ['1'; '1'; '0'; '0'; '1'; '1'] = true

let%test _ = belongsTo ['0'; '1'; '1'; '0'] = [false; true; true; false; false]
let%test _ = belongsTo ['1'; '1'; '1'] = [false; true; false; false; false]
