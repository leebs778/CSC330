use "hw3.sml";

(*
  Version 1.0

  Original code by Bassem Youssef, further modified, adapted  and extended by d. german

  This time tests are a bit different.
  This code should be relatively straight forward for you to read


  How to use:

  Once you have completed all your code, you can test it.

  Load this file then simply type in the REPL

  run();

  if you want to test only some functions, you will have to comment out tests.

  See all_tests below. Comment the body of the tests you don't want to use and then
  modify all_tests to only include the tests  you want to use.

  if a test fails then
    - use binary search to find out which one fails ;) orelse
    - create another file with each of the tests as bindings (as we did in the previous assignment) orelse
    - create your own tests orelse
    - comment out tests from their corresponding list

  This testing infrastructure is not ideal, but I borrowed code and then i extended it

  There are lots of potential improvements to this code:

  - report the failed function
  - if a positive parameter is passed to run, report successes and failures.
    if negative, report only fails.
  - report number of questions that passed, failed



 *)

fun test(test_name,tests) =
    (*  this should be written as a fold... *)
    let fun run_test(tests,passed_results,failed_results) =
            case tests of
                [] =>
                (Format.format "Results for:   %s Passed %5d Failed %5d"
                                    [ Format.LEFT (30, Format.STR test_name),
                                      Format.INT passed_results,
                                      Format.INT failed_results
                                    ],
                 if failed_results > 0 then 1 else 0)
              | (({actual,expected})::rest) => if actual = expected
                                               then run_test(rest,
                                                             passed_results+1,
                                                             failed_results)
                                               else run_test(rest,
                                                             passed_results,
                                                             failed_results+1)
    in
      run_test(tests,0,0)
    end

fun test_only_capitals() =
  let
    val words = ["This","the","A","Hello","World","not"]
  in
    test("test_only_capitals",[
          {actual=only_capitals(words),
           expected=["This","A","Hello","World"]},
          {actual=only_capitals ["A","B","C"],
           expected = ["A","B","C"]},
          {actual=only_capitals [],
           expected = []},
          {actual=only_capitals(["Mexico","Ottawa"]), expected=["Mexico", "Ottawa"]},
          {actual=only_capitals(["vancouver","Ottawa","Victoria"]), expected=["Ottawa", "Victoria"]}
        ]
        )
  end



val lwords = ["This","the","A","Hello","World","not","long string","loooong string"]
val swords = ["the","not","cat","dog"] (* check first/last etc *)

fun test_longest_string1() =
    test("test_longest_string1",[
          {actual=longest_string1(lwords),
           expected="loooong string"},
          {actual=longest_string1(swords),
           expected="the"},
          {actual=longest_string1 ["A","bc","C"], expected="bc"},
          {actual=longest_string1(["Mexico","Ottawa"]), expected="Mexico"},
          {actual=longest_string1(["vancouver","Ottawa","Victoria"]), expected="vancouver"},
          {actual=longest_string1([]), expected=""}
        ])

fun test_longest_string2() =
    test("test_longest_string2",[
          {actual=longest_string2(lwords), expected="loooong string"},
          {actual=longest_string2([]), expected=""},
          {actual=longest_string2(["Mexico","Ottawa"]), expected="Ottawa"},
          {actual=longest_string2(swords), expected="dog"}
        ])
(* (* 
fun test_longest_string3() =
    test("test_longest_string3",[
          {actual=longest_string3(lwords), expected="loooong string"},
          {actual=longest_string3(swords), expected="the"},
          {actual=longest_string3 ["A","bc","C"], expected="bc"},
          {actual=longest_string3(["Mexico","Ottawa"]), expected="Mexico"},
          {actual=longest_string3(["vancouver","Ottawa","Victoria"]), expected="vancouver"},
          {actual=longest_string3([]), expected=""}
        ])


fun test_longest_string4() =
    test("test_longest_string4",[
          {actual=longest_string4(lwords), expected="loooong string"},
          {actual=longest_string4([]), expected=""},
          {actual=longest_string4(["Mexico","Ottawa"]), expected="Ottawa"},
          {actual=longest_string4(swords), expected="dog"}
        ])

fun test_longest_cap() =
  let
    val words = ["This","the","A","Hello","World","not","long string","loooong string"]
  in
    test("longest_capitalized words",[
          {actual=longest_capitalized words, expected="Hello"},
          {actual=longest_capitalized([]), expected=""},
          {actual=longest_capitalized(["vancouver","Ottawa","Victoria","Guatemala"]), expected="Guatemala"},
          {actual=longest_capitalized ["then","The","To","testing"], expected="The"},
          {actual=longest_capitalized ["then","The","To","Cat","testing"], expected="The"},
          {actual=longest_capitalized ["then","the","to","testing"], expected=""}
        ])
    end

fun test_rev_string() =
    test("rev_string", [
          {actual=rev_string "Hello World!", expected="!dlroW olleH"},
          {actual=rev_string "canada", expected="adanac"},
          {actual=rev_string "anna", expected="anna"},
          {actual=rev_string "", expected=""
        }])

fun test_first_answer() =
    test("first_answer", [
          {actual=first_answer (fn x => if (x mod 2) = 0 then SOME x else NONE) [1,1,4,3],expected=4},
          {actual=first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5], expected=4}
        ])

fun test_first_answer_exc() =
    test("first_answer_exception",[
          {actual=(first_answer
                       (fn x => if (x mod 2) = 0 then SOME x else NONE) [1,1,5,3]; false)
           handle NoAnswer => true,
           expected=true},
          {actual=(first_answer (fn x => if x > 3 then SOME x else NONE) [];false)
           handle NoAnswer => true,
           expected=true}
        ])

fun test_all_answers() =
    test("all_answers",
    [{actual=all_answers (fn x => if (x < 0) then NONE else SOME ([1,2,3])) [1,2,3],
      expected=SOME [1,2,3,1,2,3,1,2,3]},
    {actual=all_answers (fn x => if (x < 0) then NONE else SOME ([1])) [1,2,3],
      expected=SOME [1,1,1]},
    {actual=all_answers (fn x => if (x < 0) then NONE else SOME ([x])) [1,2,3],
      expected=SOME [1,2,3]},
    {actual=all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7], expected=NONE},
    {actual=all_answers (fn x => if x = 1 then SOME [x] else NONE) [1,1,1,1,1], expected=SOME [1,1,1,1,1]},
    {actual=all_answers (fn x => if x = 1 then SOME [x] else NONE) [1,2,1,2,1,2], expected=NONE},
    {actual=all_answers (fn x => if (x < 0) then NONE else SOME ([1])) [],
      expected=SOME []}])

fun test_count_wildcards () =
    test("count_wildcards",
    [{actual=count_wildcards(Wildcard),expected=1},
     {actual=count_wildcards(UnitP), expected=0},
     {actual=count_wildcards(ConstP 1),expected=0},
     {actual=count_wildcards(TupleP []),expected= 0},
     {actual=count_wildcards(TupleP [Wildcard]),expected=1},
     {actual=count_wildcards(TupleP [Wildcard, UnitP, Variable "x"]),expected=1},
     {actual=count_wildcards(TupleP [Wildcard, Wildcard, Variable "xy"]),expected=2},
     {actual=count_wildcards(TupleP [TupleP [Wildcard, Wildcard]]),expected=2},
     {actual=count_wildcards(TupleP [ConstructorP ("a", Wildcard), TupleP [Wildcard]]),
      expected=2}])

fun test_count_some_var() =
  test("count_some_var",
       [{actual=count_some_var("test",
              TupleP [TupleP [Wildcard, UnitP, Variable "test",Variable "test"]]),
         expected=2},
       {actual=count_some_var("test",TupleP [TupleP [Wildcard, UnitP,
                              Variable "te"]]),
         expected=0},
       {actual=count_some_var ("test",
                  TupleP [TupleP [Wildcard, UnitP, Variable "test"]]),
         expected=1}])

fun test_check_pat() =
    test("check_pat",
         [{actual=check_pat (TupleP [Wildcard,Variable "cat",
                            Variable "pp",TupleP[Variable "tt"],
                            Wildcard,ConstP 3,
                            ConstructorP("cony",Variable "pp")]),
           expected=false},
         {actual=check_pat (TupleP [Wildcard,Variable "cat",
                            Variable "pp",TupleP[Variable "tt"],
                            Wildcard,ConstP 3,
                            ConstructorP("cony",Variable "test")]),
           expected=true}])

fun test_match() =
    test("test_match",
         [{actual=match(Unit, UnitP),
           expected=SOME []},
          {actual=match(Unit, Variable "cat"),
           expected=SOME [("cat", Unit)]},
          {actual=match(Unit, ConstP 3),
           expected=NONE},
          {actual=match(Tuple [], TupleP [Wildcard]),
           expected=NONE},
          {actual=match(Tuple [Unit], TupleP []),
           expected=NONE},
          {actual=match(Tuple [Unit], Variable "cat"),
           expected=SOME [("cat", Tuple [Unit])]},
          {actual=match(Tuple [Unit], TupleP [Variable "cat"]),
           expected=SOME [("cat", Unit)]},
          {actual=match(Tuple [Unit, Const 8], TupleP [Variable "cat", ConstP 3]),
           expected=NONE},
          {actual=match(Tuple [Unit, Const 8], TupleP [Variable "cat", Variable "dog"]),
           expected=SOME [("cat", Unit),("dog", Const 8)]},
          {actual=match(Tuple [Unit, Tuple [Unit, Unit]],
                        TupleP [Variable "cat", TupleP [Variable "dog", Variable "rat"]]),
           expected=SOME [("cat", Unit), ("dog", Unit),  ("rat", Unit)]},
          {actual=match(Constructor ("mat", Unit), ConstructorP ("hat", Variable "cat")),
           expected=NONE},
          {actual=match(Constructor ("dog", Unit), ConstructorP ("dog", Variable "cat")),
           expected=SOME [("cat", Unit)]},
          {actual=match(Tuple[Const 17, Unit, Const 7,
                              Constructor ("zoe", Const 7),
                              Constructor ("zoe", (Constructor ("zoe", Const 7)))],
                        TupleP[Wildcard,Wildcard]),
           expected=NONE},
          {actual=match(Const 7, Wildcard ),
           expected=SOME []},
          {actual=match(Unit, Wildcard ),
           expected=SOME []},
          {actual=match(Tuple[Const 7], Wildcard ),
           expected=SOME []},
          {actual=match(Constructor("cat", Const 7), Wildcard ),
           expected=SOME []},
          {actual=match(Const 7, Variable "Zoe" ),
           expected=SOME [("Zoe", Const 7)]},
          {actual=match(Unit, Variable "chopsticks" ),
           expected=SOME [("chopsticks", Unit)]},
          {actual=match(Unit, UnitP ),
           expected=SOME []},
          {actual=match(Const 7, UnitP ),
           expected=NONE},
          {actual=match(Const 7, ConstP 7 ),
           expected=SOME []},
          {actual=match(Const 7, ConstP 8 ),
           expected=NONE},
          {actual=match(Constructor("Cat", Const 7), ConstructorP("Cat", Wildcard)),
           expected= SOME[]},
          {actual=match(Constructor("Dog", Const 7), ConstructorP("Cat", Wildcard)),
           expected= NONE},
          {actual=match(Constructor("Cat", Const 7), ConstructorP("Cat", UnitP)),
           expected= NONE},
          {actual=match(Constructor("Cat", Const 7), ConstructorP("Cat", Variable "dog")) ,
           expected= SOME [("dog", Const 7)]},
          {actual=match(Tuple[Const 7], TupleP[ConstP 7]),
           expected= SOME []},
          {actual=match(Tuple[Const 7], TupleP[ConstP 7,ConstP 7]),
           expected= NONE},
          {actual=match(Tuple[Const 7, Const 6, Unit, Const 8],
                        TupleP[ConstP 7, Variable "cat",Wildcard, ConstP 8]),
           expected=SOME [("cat",Const 6)]},
          {actual=match(Tuple[Const 7, Const 6, Unit, Const 7],
                        TupleP[
                          Variable "a",
                          Variable "ab",
                          Variable "abc",
                          Variable "abcd"]),
           expected=SOME [("a",Const 7), ("ab",Const 6), ("abc",Unit), ("abcd",Const 7)
          ]}
         ])

fun test_first_match() =
    test("test_first_match",
         [
           {actual=first_match Unit [UnitP], expected=SOME []},
           {actual=first_match Unit [Variable "cat"],
            expected=SOME [("cat", Unit)]},
           {actual=first_match Unit [ConstP 3],
            expected=NONE},
           {actual=first_match (Tuple []) [TupleP [Wildcard]],
            expected=NONE},
           {actual=first_match (Tuple [Unit]) [TupleP []],
            expected=NONE},
           {actual=first_match (Tuple [Unit]) [TupleP [Variable "cat"]],
            expected=SOME [("cat", Unit)]},
           {actual=first_match (Tuple [Unit, Const 8]) [TupleP [Variable "cat", Variable "dog"]],
            expected=SOME [("cat", Unit),("dog", Const 8)]},
           {actual=first_match (Constructor ("mat", Unit)) [ConstructorP ("hat",Variable "cat")],
            expected=NONE},
           {actual=first_match (Constructor ("dog", Unit)) [ConstructorP("dog",Variable "cat")],
            expected=SOME [("cat", Unit)]},
           {actual=first_match (Const 7) [Wildcard ],
            expected=SOME []},
           {actual=first_match Unit [Wildcard ],
            expected=SOME []},
           {actual=first_match (Tuple[Const 7]) [Wildcard ],
            expected=SOME []},
           {actual=first_match Unit [Variable "chopsticks" ],
            expected=SOME [("chopsticks", Unit)]},
           {actual=first_match Unit [UnitP ],
            expected=SOME []},
           {actual=first_match (Const 7) [UnitP ],
            expected=NONE},
           {actual=first_match (Const 7) [ConstP 7 ],
            expected=SOME []},
           {actual=first_match (Const 7) [ConstP 8 ],
            expected=NONE},
           {actual=first_match (Constructor("Cat", Const 7)) [ConstructorP("Cat",Variable "dog")] ,
            expected= SOME [("dog",Const 7)]},
           {actual=first_match (Tuple[Const 7]) [TupleP[ConstP 7]],
            expected= SOME []},
           {actual=first_match (Tuple[Const 7]) [TupleP[ConstP 7,ConstP 7]],
            expected= NONE},
           {actual=first_match (Tuple[Const 7, Const 6, Unit, Const 8])
                               [TupleP[ConstP 7, Variable "cat",Wildcard, ConstP 8]],
            expected=SOME [("cat",Const 6)]},
           {actual=first_match (Tuple[Const 7, Const 6, Unit, Const 7])
                               [TupleP[Variable "a", Variable "ab",
                                       Variable "abc", Variable "abcd"]],
            expected=SOME [
              ("a",Const 7),
              ("ab",Const 6),
              ("abc",Unit),
              ("abcd",Const 7)
           ]}
        ])

val all_tests =
    [test_only_capitals,
     test_longest_string1,
     test_longest_string2,
     test_longest_string3,
     test_longest_string4,
     test_longest_cap,
     test_rev_string,
     test_first_answer,
     test_first_answer_exc,
     test_all_answers,
     test_count_wildcards ,
     test_count_some_var,
     test_check_pat,
     test_match,
     test_first_match
     ]
*)*)
val all_tests =
    [test_only_capitals,
     test_longest_string1,
     test_longest_string2]

fun run() =
    let
      val testResults = map (fn x => x()) all_tests
      val failed = foldl (fn ((st,failed),acc) => (print (st ^ "\n"); failed + acc)) 0 testResults
    in
      print (Format.format "\n%d failed tests\n\n" [ Format.INT failed])
    end
