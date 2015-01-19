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
(*
val ClubAce = (Clubs,Ace)
val DiamondsJack = (Diamonds,Jack)
val Hearts10 = (Hearts, Num 10)
val Spades5 = (Spades,Num 5)
								*)

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
fun sum_cards(cs: card list): int =
	let
		fun helper(cs, acc) =
			case cs of 
				[] => acc
			|	x::xs => helper(xs, acc+card_value(x))
	in
		helper(cs, 0)
	end

(* 10 *)
fun score(cs: card list, goal: int): int =
	let
		val sum = sum_cards(cs)
		fun prelim (sum) = 
			if (sum > goal) 
			then 2 * (sum - goal)
			else goal - sum
	in
		if (all_same_color(cs))
		then prelim(sum) div 2
		else prelim(sum)
	end

(* 11 *)
fun officiate(cl: card list, ml: move list, goal: int): int =
	let fun loop(cl: card list, hand: card list, ml: move list): int = 
			if (sum_cards(hand) > goal)
			then (score(hand, goal))
			else 
				(case ml of
					[] => score(hand,goal)
				| 	Discard card::tail => loop(cl,remove_card(hand,card,IllegalMove),tail)
				|	Draw::tail =>( case cl of
										[] => score(hand,goal) 		(* no cards left to draw*)
									|	a::ab => loop(ab,a::hand,tail) 
								) 
									
				)
	in
		loop(cl,[],ml)
	end;














