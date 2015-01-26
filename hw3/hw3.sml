(* Assign 03 Provided Code *)

(*  Version 1.0 *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end


(**** put all your code after this line ****)

(* Assignment done by Peter Lebo for Daniel German's CSC 330 offering in Spring 2015 at UVic *)
(* Last edited 1/25/15 *)

(* 1 *)
fun only_capitals (l1: string list): string list = 
	List.filter (fn l2 => Char.isUpper(String.sub((l2),0))) l1

(* 2 *)
fun longest_string1(l1: string list): string = 
	List.foldl (fn (x, y) => if String.size x > String.size y then x else y) "" l1

(* 3 *)	
fun longest_string2(l1: string list): string = 
	List.foldl (fn (x,y) => let
								val xLen = String.size x
								val yLen = String.size y
							in
								if xLen >= yLen 
								then x
								else y
							end ) "" l1

(* 4 *)
fun longest_string3(l1: string list): string = 
	
fun longest_string4(l1: string list): string=

fun longest_string_helper














