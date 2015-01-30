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




(**** put all your code after this line ****)

(* Assignment done by Peter Lebo for Daniel German's CSC 330 offering in Spring 2015 at UVic *)
(* Last edited 1/25/15 *)

(* 1 *)
fun only_capitals (l1: string list): string list = 
	List.filter (fn l2 => Char.isUpper(String.sub((l2),0))) l1

(* 2 *)
fun longest_string1(l1: string list): string = 
	foldl (fn (x, y) => if String.size x > String.size y then x else y) "" l1

(* 3 *)	
fun longest_string2(l1: string list): string = 
	foldl (fn (x,y) =>	if String.size x >= String.size y then x else y) "" l1


(* 4 *)

fun longest_string_helper f l1 = 
	List.foldl (fn (x,y) => if f(String.size x, String.size y) then x else y) "" l1


val longest_string3 = longest_string_helper (fn (x,y) => x > y);
val longest_string4 = longest_string_helper (fn (x,y) => x >= y);


(* 5 *)
fun longest_capitalized l1 = 
	(longest_string1 o only_capitals) l1

(* 6 *)
fun rev_string s1 = 
	(String.implode o List.rev o String.explode) s1
	
(* 7 *)
fun first_answer f xs = 
	case xs of
		[] => raise NoAnswer
	|	x::xs' => (case f x of
						NONE => first_answer f xs'
					|	SOME x => x)

(* 8 *)
fun all_answers f xs = 
	let
		fun all_helper (f, xs, acc) =
			case xs of
				[] => SOME acc
			|	x::xs' => case f(x) of
							NONE => NONE
						|	SOME y => all_helper(f,xs',acc@y)
	in
		all_helper(f,xs,[])
	end

(* 9a *)

(*
		function G takes 2 functions f1 and f2 and then an element of the constructors pattern or valu
			depending on the p passed into it, it takes different actions responding to the case on p
*) 

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



(* 9b *) 
	
fun count_wildcards (p: pattern) = 
	g (fn _ => 1) (fn _ => 0) p

(* 9c *)
fun count_wild_and_variable_lengths (p: pattern) = 
	g (fn _ => 1) (fn x => String.size x) p

(* 9d *)
fun count_some_var (s: string, p: pattern) = 
	g (fn _ => 0) (fn x => if s = x then 1 else 0) p

(* 10 *)
fun check_pat (p: pattern) = 
	let 
		fun diffG patt = 
			case patt of
				Variable x => [x]
			|	TupleP ps => List.foldl (fn (p,i) => i @ (diffG p)) [] ps
			|	ConstructorP(_,p) => diffG p
			|	_ => []

		fun check l1 =
			case l1 of
				[] => true
			|	x::xs => (not(List.exists (fn y => y = x) xs)) andalso check(xs)

	in 
		check (diffG p)
	end

(* 11 *)
(* 12 *)















