(* CSE 130: Programming Assignment 2
 * misc.ml
 * Author: Kevin Yang
 * Login name: kjy002
 *)

(* assoc: int * string * (string * int) list -> int
 * assoc takes a triple (d,k,l) where l is a list of key value pairs 
 * [(k1,v1);(k2,v2);...] and finds the first ki that equals k. If such a ki is
 * found, then vi is returned. Otherwise, the default value d is returned.
 *)

let rec assoc (d,k,l) =
  match l with 
    | [] -> d
    | (a,b)::t -> if (k = a) then b else assoc (d,k,t@[])
;;
    

(* removeDuplicates: int list -> int list 
 * removeDuplicates takes a list l and returns the list of elments l with the
 * duplicates, i.e. second, third, etc. occurences, removed, and where the
 * remaining elements appear in the same order as in l.
 *)

let removeDuplicates l = 
  let rec helper (seen,rest) = 
      match rest with 
        [] -> seen
      | h::t -> 
        let seen' = 
	  if ((List.mem h rest) && (not (List.mem h seen))) then h::seen 
	  else seen in
        let rest' = t in 
	  helper (seen',rest') 
  in
      List.rev (helper ([],l))
;;


(* wwhile : (int -> int * bool) * int -> int
 * wwhile takes as input a pair (f,b) and calls the function f on input b to 
 * get a pair (b',c'). wwhile should continue calling f on b' to update the 
 * pair as long as c' is true. Once f returns a c' that's false, wwhile should
 * return b'.
 *)

let rec wwhile (f,b) = 
  let (x,y) = f b in (if (not y) then x else wwhile (f,x));;

(* fixpoint : (int -> int) * int -> int
 * fixpoint repeatedly updates b with f(b) until b=f(b) and then returns b.
 *)

let fixpoint (f,b) = wwhile ((let g b = (f b,not ((f b = b))) in g),b);;


(* ffor: int * int * (int -> unit) -> unit
   Applies the function f to all the integers between low and high
   inclusive; the results get thrown away.
 *)

let rec ffor (low,high,f) = 
  if low>high 
  then () 
  else let _ = f low in ffor (low+1,high,f)
      
(************** Add Testing Code Here ***************)

