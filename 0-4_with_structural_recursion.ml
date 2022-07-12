(* exercises 0-4 with structural recursion*)

(* https://ocaml.org/problems *)

(* 12 July 2022*)

(* Zhu Wentao *)

(* ************** *)

(* tail of a list *)

let exercise_00 = "mandatory";;

let test_last candidate =
  (candidate ["a" ; "b" ; "c" ; "d"] = Some "d")
  && (candidate ["a"] = Some "a")
  && (candidate [] = None);;

let rec last vs =
  match vs with
  | [] -> None
  | [x] -> Some x
  | first_el :: rest_of_list -> last rest_of_list;;
         

let () = assert(test_last last);;

(* last two elements of a list *)

let exercise_01 = "mandatory";;

let test_last_two candidate =
  (candidate ["a" ; "b" ; "c" ; "d"] = Some ("c", "d"))
  && (candidate ["a"] = None)
  && (candidate ["a" ; "b"] = Some ("a", "b"));;

let rec last_two vs =
  match vs with        
  | [] | [_] -> None
  | [x ; y] -> Some (x,y)
  | _ :: t -> last_two t;;

let () = assert (test_last_two last_two);;

(* N'th element of a list *)

let exercise_02 = "mandatory"

let test_at candidate =
  (candidate 2 ["a" ; "b" ; "c"] = Some "c")
  && (candidate 2 ["a"] = None)
  && (candidate 0 ["a"] = Some "a")
  && (candidate 5 ["a"] = None);;

let rec at k vs =
  match vs with
  | [] -> None
  | first :: rest ->
     if k = 0 then Some first
     else at (k-1) rest;;

  
let () = assert (test_at at);;

(* length of a list *)

let exercise_03 = "mandatory";;

let test_length candidate =
  (candidate ["a" ; "b" ; "c"] = 3)
  && (candidate [] = 0);;

let rec length list =
  match list with
  | [] -> 0
  | x :: xs -> length xs +1;;


let () = assert (test_length length);;

(* reverse a list *)

let exercise_04 = "mandatory";;

let test_rev candidate =
  (candidate ["a" ; "b" ; "c"] = ["c" ; "b" ; "a"])
  && (candidate ["a"] = ["a"])
  && (candidate [] = []);;

let rec rev list =
  let rec reverse list a =
    match list with
    | [] ->
       a
    | v :: vs' ->
       reverse vs' (v :: a)
  in reverse list [];;
   
let () = assert (test_rev rev);;
   
     
(*
# # use "/Users/zwt2000/Desktop/solve ocaml problems/0-4_with_structural_recursion.ml";;
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
