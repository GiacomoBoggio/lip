(* val addlist : int list -> int *)
(* addlist l adds the element of the list of integers l *)

let rec addlist l = match l with
| [] -> 0 (* Base case: the sum of an empty list is 0 *)
| x :: xs -> x + addlist xs (* Sum the head with the result of the recursive call on the tail *)
