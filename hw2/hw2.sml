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
fun get_substitutions2(s1: string list list, s: string): string list =
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
		val {first = x, middle = y, last = z} = name
		fun eval(name: string list) =
			case name of
				[] => []
			|	m::ms => {first=m, middle=y, last=z} :: eval(ms)
	in 
		case get_substitutions2(s1, x) of
				[] => [name]
			|	x => name :: eval(x)
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
(* TEST VALUES *)
(* DELETE BEFORE SUBMISSION *)

val ClubAce = (Clubs,Ace)
val DiamondsJack = (Diamonds,Jack)
val Hearts10 = (Hearts, Num 10)
val Spades5 = (Spades,Num 5)

exception notFound

val cards1 = [(Clubs, Ace), (Diamonds, Num 10), (Spades, Num 4), (Clubs, Num 4)]
val cards2 = []
val cards3 = [(Clubs, Ace), (Diamonds, Num 10), (Spades, Num 5), (Clubs, Num 9)]
val cards4 = [(Clubs, Ace), (Clubs, Num 10), (Clubs, Num 5), (Clubs, Num 2)]
val cards5 = [(Diamonds, Ace), (Diamonds, Num 10), (Diamonds, Queen), (Diamonds, Jack), (Diamonds,King)]


(* 5 *)
fun card_color(c: card): color =
	case c of
		(Clubs,_) => Black
	|	(Diamonds,_) => Red
	|	(Hearts,_) => Red
	| 	(Spades,_) => Black

(* 6 *)
fun card_value(c: card): int = 
	case c of
		(_,Ace) => 11
	|	(_,Num 2) => 2
	|	(_,Num 3) => 3
	|	(_,Num 4) => 4
	|	(_,Num 5) => 5
	|	(_,Num 6) => 6
	|	(_,Num 7) => 7
	|	(_,Num 8) => 8
	|	(_,Num 9) => 9
	| 	(_,_) => 10

(* 7 *)
(* weird error on first test case 7_1a -- I thought it was fixed ... hit up Daniel about it? *)
fun remove_card(cs: card list, c: card, e): card list = 
	case cs of
		[] => raise e
	|	x::xs => 	if x = c 
					then xs
					else x :: remove_card(xs,c,e)

(* 8 *)
fun all_same_color(cs: card list): bool = 
	let
		fun comp_color(l1: card list) = 
			case l1 of
				[] => true
			| 	_::[] => true
			|	x::y::xs => if (card_color(x) = card_color(y))
							then comp_color(y::xs)
							else false
	in
		comp_color(cs)
	end

(* 9 *)

(* 10 *)
(* 11 *)
















