(* if you use this function to compare two strings (returns true if the same
   string), then you avoid some warning regarding polymorphic comparison  *)


fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for Part 1 here *)

(* 1 *) 
(* Clean up! *)
fun all_except_option(s1: string, l1: string list): string list option = 
	case l1 of
		[] => NONE
	|	x::xs => if same_string(s1,x) 
					then SOME (xs)
					else case all_except_option(s1,xs) of
						NONE => NONE
					|	SOME s => SOME (x::s)

(* 2 *)
fun get_substitutions1(s1: string list list, s: string): string list = 
	case s1 of
		[] => []
	|	x::xs => case all_except_option(s,x) of 
						NONE => get_substitutions1(xs,s)
					| 	SOME a => a @ get_substitutions1(xs,s)

(* 3 *)
fun get_substitutions1(s1: string list list, s: string): string list =
	let 
		fun tRecursion(s1: string list list, l1: string list) = 
			case s1 of
				[] => l1
			|	x::xs => case all_except_option(s,x) of
								NONE => tRecursion(xs,l1)
							|	SOME a => tRecursion(xs,l1@a)
	in
		tRecursion(s1,[])
	end

(* 4 *)
fun similar_names(s1: string list list, name: {first:string , middle:string, last:string}): {first:string, middle:string, last:string} list = 
	let
		fun local_helper(l1: string list list) = 
			case s1 of
				[] => []
			|	x::xs => case all_except_option()

	in
		local_helper(s1)
	end
	


(************************************************************************)
(* Game  *)

(* you may assume that Num is always used with valid values 2, 3, ..., 10 *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw


exception IllegalMove

(* put your solutions for Part 2 here *)
