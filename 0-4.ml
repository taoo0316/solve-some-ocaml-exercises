(* exercises 0-4 *)

(* https://ocaml.org/problems *)

(* 12 July 2022*)

(* Zhu Wentao *)

(* ************** *)

(* tail of a list *)

let exercise_00 = "mandatory";;

let test_last candidate =
     (candidate ["a" ; "b" ; "c" ; "d"] = Some "d")
     && (candidate [] = None);;

let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: t -> last t;;

let () = assert(test_last last);;

(* last two elements of a list *)

let exercise_01 = "mandatory";;

let test_last_two candidate =
  (candidate ["a" ; "b" ; "c" ; "d"] = Some ("c", "d"))
  && (candidate ["a"] = None);;

let rec last_two = function
  | [] | [_] -> None
  | [x ; y] -> Some (x,y)
  | _ :: t -> last_two t;;

let () = assert (test_last_two last_two);;

(* N'th element of a list *)

let exercise_02 = "mandatory"

let test_at candidate =
  (candidate 2 ["a" ; "b" ; "c"] = Some "c")
  &&(candidate 2 ["a"] = None);;

let rec at k = function
  | [] -> None
  | h :: t -> if k = 0 then Some h else at (k-1) t;;

let () = assert (test_at at);;

(* length of a list *)

let exercise_03 = "mandatory";;

let test_length candidate =
  (candidate ["a" ; "b" ; "c"] = 3)
  && (candidate [] = 0);;

let length list =
  let rec aux n = function
    | [] -> n
    | _ :: t -> aux (n+1) t
  in
  aux 0 list;;

let () = assert (test_length length);;

(* reverse a list *)

let exercise_04 = "mandatory";;

let test_rev candidate =
  (candidate ["a" ; "b" ; "c"] = ["c" ; "b" ; "a"])
   && (candidate [] = []);;

let rev list =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: acc) t
  in
  aux [] list;;
   
let () = assert (test_rev rev);;
   
     
(*
# # use "/Users/zwt2000/Desktop/solve ocaml problems/0-4.ml";;
val exercise_00 : string = "mandatory"
val test_last : (string list -> string option) -> bool = <fun>
val last : 'a list -> 'a option = <fun>
val exercise_01 : string = "mandatory"
val test_last_two : (string list -> (string * string) option) -> bool = <fun>
val last_two : 'a list -> ('a * 'a) option = <fun>
val exercise_02 : string = "mandatory"
val test_at : (int -> string list -> string option) -> bool = <fun>
val at : int -> 'a list -> 'a option = <fun>
val exercise_03 : string = "mandatory"
val test_length : (string list -> int) -> bool = <fun>
val length : 'a list -> int = <fun>
val exercise_04 : string = "mandatory"
val test_rev : (string list -> string list) -> bool = <fun>
val rev : 'a list -> 'a list = <fun>
#
*)
