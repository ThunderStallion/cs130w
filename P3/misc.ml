(* CSE 130: Programming Assignment 3
 * misc.ml
 * Author: Kevin Yang
 * Login name: kjy002
 *)

(* For this assignment, you may use the following library functions:

   List.map
   List.fold_left
   List.fold_right
   List.split
   List.combine
   List.length
   List.append
   List.rev

   See http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html for
   documentation.
*)



(* Do not change the skeleton code! The point of this assignment is to figure
 * out how the functions can be written this way (using fold). You may only
 * replace the   failwith "to be implemented"   part. *)



(*****************************************************************)
(******************* 1. Warm Up   ********************************)
(*****************************************************************)

(* sqsum : int list -> int
 * sqsum takes a list of integers [x1;...;xn] and returns the integer: x1^2 +
 * ...+xn^2.
 *)

let sqsum xs = 
  let f a x = (a) + (x*x) in
  let base = 0 in
    List.fold_left f base xs

(* pipe : ('a -> 'a) list -> ('a -> 'a)
 * pipe takes a list of functions [f1;...fn] and returns a function f such that
 * for any x, the appliation f x returns the result fn(...(f2(f1 x))).
 *)

let pipe fs = 
  let f a x = let g(i) = x(a(i)) in g in
  let base = let f(i) = i in f in
    List.fold_left f base fs

(* sepConcat : string -> string list -> string
 * The function sepConcat is a curried function which takes as input a string
 * sep to be used as a separtoor, and a list of strings [s1;...;sn]. If there
 * are 0 strings in the list, then sepConcat should return "". If there's 1
 * string in the list, then sepconcat should return s1. Otherwise, sepConcat
 * should return the concatination s1 sep s2 sep s3 ... sep sn.
 *)

let rec sepConcat sep sl = match sl with 
  | [] -> ""
  | h :: t -> 
      let f a x = if (x = h) then x else a^sep^x in
      let base = sep in
      let l = sl in
        List.fold_left f base l

(* stringOfList : ('a -> string) -> 'a list -> string
 * The first input is a function f: 'a -> string which will be called by
 * stringOfList to convert each element of the list to a string. The second
 * input is a list: 'a list, which we'll think of as having the elements
 * l1,l2,..,ln. stringOfList should return a string representation of the list
 * as a concatenation of the following: "["
 * (f|1)";"(f|2)";"(f|3)";"...";"(f|n)"]"
 *)

let stringOfList f l = "["^(sepConcat "; " (List.map f l))^"]"

(*****************************************************************)
(******************* 2. Big Numbers ******************************)
(*****************************************************************)

(* clone : 'a -> int -> 'a list
 * clone takes as input x and then takes as input an integer n. The result is a
 * list of length n, where each element is x. If n is 0 or negative, clone
 * should return the empty list.
 *)

let rec clone x n = if (n <= 0) then [] else [x]@(clone x (n-1))

(* padZero : int list -> int list -> int list * int list
 * clone takes two lists: [x1,...,xn] [y1,...,ym] and adds zero in front to make
 * the lists equal.
 *)

let rec padZero l1 l2 = 
  if ((List.length l1) = (List.length l2)) then (l1,l2) else 
    if ((List.length l1) < (List.length l2)) then (((clone 0 ((List.length l2) - (List.length l1)))@l1),l2)
      else (l1, ((clone 0 ((List.length l1) - (List.length l2)))@l2))

(* removeZero : int list -> int list
 * removeZero takes a list and removes a prefix of trailing zeros.
 *)

let rec removeZero l = match l with
  | [] -> []
  | h::t -> if (not(h = 0)) then l else removeZero t

(* bigAdd : int list -> int list -> int list
 * bigAdd takes two integer lists, where each integer is in the range [0,9] and
 * returns the list corresponding to the addition of the two big integers.
 *)

let bigAdd l1 l2 = 
  let add (l1, l2) = 
    let f a x = match x with
      | (c,d) -> match a with
        | (carry,result) -> match result with 
          | [] -> if ((carry+c+d) < 10) then (0,[carry]@[carry+c+d]) else (carry+1,[carry+1]@[(carry+c+d) mod 10])
          | h::t -> if ((c+d+h) < 10) then (0,[0]@[c+d+h]@(t)) else (carry+1,[(h+c+d)/10]@[(h+c+d) mod 10]@t)
  in 
    let base = (0, []) in
    let args = List.rev (List.combine l1 l2) in
    let (_, res) = List.fold_left f base args in
      res
  in 
    removeZero (add (padZero l1 l2))

(* mulByDigit : int -> int list -> int list 
 * mulByDigit takes an integer digit and a big integer, and returns the big
 * integer list which is the result of multiplying the big integer with the
 * digit.
 *)

let rec mulByDigit i l = match i with
  | 0 -> [0]
  | 1 -> l
  | _ -> bigAdd l (mulByDigit (i-1) l) 

(* bigMul : int list -> int list -> int list
 *)

let bigMul l1 l2 = 
  let f a x = match x with
    | (c,d) -> match a with
      | (carry,result) -> ((((c*d) + carry)/10),(bigAdd (mulByDigit d l1))
      (result@[0]))
  in
  let base = (0,[]) in
  let args = List.combine (fst(padZero l1 l2)) (snd(padZero l1 l2)) in
  let (_, res) = List.fold_left f base args in
    res

