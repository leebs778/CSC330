(*  Assignment #1 *)
(* Code written by Peter Lebo - V00748436 - CSC 330 - Spring 2015 *)

type DATE = {year:int, month:int, day: int}
exception InvalidParameter

(* This file is where your solutions go *)

(* Write a function is_older that takes two dates and evaluates to true or false. It evaluates to true if the first argument is a date that comes after the second argument. (If the two dates are the same, the result is false. *)

val feb28_2012 = {year = 2012, month = 2, day = 28}
val dec1_2013 = {year = 2013, month = 12, day = 1}
val march31_2011 = {year = 2011, month = 3, day = 31}
val april28_2011 = {year = 2011, month = 4, day = 28}
val june1_2013 = {year = 2013, month = 6, day = 1}
val dec31_2013 = {year = 2013, month = 12, day = 31}
val jan1_2014 = {year = 2014, month = 1, day = 1}


(* 1 *)
fun is_older(d1: DATE, d2: DATE): bool =
    let
        (* date1 and date2 converted into 'day' units *)
        (* year units are considered 403 because we use 1 indexing and (12*31)months = 372 + 31 = 403 *)
        val date1 = #year d1 * 403 + #month d1 * 31 + #day d1
        val date2 = #year d2 * 403 + #month d2 * 31 + #day d2
    in
        date1 > date2
    end


(* 2 *)
fun number_in_month(l1: DATE list, num: int): int = 
    if (null l1) then 0
    else 
        let 
            val head = hd l1
            val mon = #month head
        in  
            if (mon = num) then 1 + number_in_month(tl l1, num)
            else number_in_month(tl l1, num)
        end

(* 3 *)
fun number_in_months(l1: DATE list, l2: int list): int = 
    if null(l2) then 0
    else 
    number_in_month(l1, hd l2) + number_in_months(l1, tl l2)


(* 4 *)
fun dates_in_month(l1: DATE list, month: int): DATE list =
    if null(l1)
    then []
    else 
        if month = #month (hd l1)
        then (hd l1)::(dates_in_month(tl l1,month))
        else 
            dates_in_month(tl l1,month)

(* 5 *)
fun dates_in_months(l1: DATE list, l2: int list): DATE list = 
    if null (l2)
    then []
    else dates_in_month(l1, hd l2) @ dates_in_months(l1, tl l2)


(* 6 *)
fun get_nth(l1: string list, num: int): string = 
    if num = 0 orelse length (l1) < num
        then raise InvalidParameter
    else
        let 
            fun iterate(l1: string list, current: int): string = 
                if (current <> num)
                then iterate(tl l1, current+1)
                else hd l1
        in
            iterate(l1,1)
        end

(* 7 *)
fun date_to_string(d1: DATE) = 
    let
        val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in 
        get_nth(months, #month d1) ^ " " ^ Int.toString(#day d1) ^ ", " ^ Int.toString(#year d1)
    end;

(* 8 *)
fun number_before_reaching_sum(sum: int, l1: int list): int = 
    let 
        fun getSum(sum2: int, l2: int list, subTotal: int, index: int): int =    
            if null (l2) 
            then index
            else
            if (hd l2 + subTotal) < sum2
            then getSum(sum2, tl l2, subTotal+hd l2, index + 1)
            else index
    in
        getSum(sum, l1, 0, 0)
    end

(* 9 *)
fun what_month(day: int): int = 
    let
        val days_in_Month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        fun calculate(day2: int, days_in_Month: int list, index: int): int = 
            if(day2 > hd days_in_Month)
            then calculate(day2-hd days_in_Month, tl days_in_Month, index + 1)
            else index
    in 
        calculate(day, days_in_Month, 1)
    end

(* 10 *)
fun month_range(d1: int, d2: int): int list= 
    if d1 > d2
    then []
    else
        let
            val ans = []
            val diff = d2 - d1 + 1
            fun loop(d1:int, d2: int): int list=
                if d1 > d2
                then []
                else  what_month(d1)::loop(d1+1,d2)
        in
            loop(d1,d2)
        end 

(* 11 *)
fun oldest(l1: DATE list) = 
    if null (l1)
    then NONE
    else
        let
            fun findMax(l1: DATE list, currentMax: DATE): DATE = 
                if null (l1)
                then currentMax
                else
                    if is_older(hd l1, currentMax)
                    then findMax(tl l1, hd l1)
                    else findMax(tl l1, currentMax)
        in
            SOME(findMax(tl l1, hd l1))
        end
 
(* 12 *)

(* i turned this into two separate functions for clarity's sake*)
 fun isLeap(year: int): string list =
                if (year mod 400 = 0) 
                then ["31", "29", "31", "30", "31", "30", "31", "31", "30", "31", "30", "31"]
                else 
                    if year mod 100 = 0 
                    then ["31", "28", "31", "30", "31", "30", "31", "31", "30", "31", "30", "31"]
                    else    
                        if year mod 4 = 0 
                        then ["31", "29", "31", "30", "31", "30", "31", "31", "30", "31", "30", "31"]
                        else ["31", "28", "31", "30", "31", "30", "31", "31", "30", "31", "30", "31"]


fun reasonable_date(d1: DATE): bool = 
    if #year d1 <= 0
    then false
    else
        if #month d1 = 0 orelse #month d1 > 12
        then false
        else 
            let
                val days_in_Month = isLeap(#year d1)
            in
                if (#day d1 <= valOf(Int.fromString(get_nth(days_in_Month, #month d1))))
                then true
                else false
            end





