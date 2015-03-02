#lang racket

;; Version 1.2
;;
;; 2015-02-21  dmg  <dmg@uvic.ca>
;;
;;	* Moved mlet* tests to part C.
;;
;; 2015-02-20  dmg  <dmg@uvic.ca>
;;
;;      * Created tests cases from many different sources.
;;

(require rackunit "hw5.rkt")
(require rackunit/text-ui)

(define hw5-tests-part-A
  (test-suite
   "Tests for HW 5 Part A"

   (test-equal? "MUPL list -> Racket list #1"
                (mupllist->racketlist (apair (int 3) (apair (int 4) (apair (int 9) (aunit)))))
                (list (int 3) (int 4) (int 9))
                )

   (test-equal? "MUPL list -> Racket list #2"
                (racketlist->mupllist (list
                                       (list (int 42) (var "x"))
                                       (list (int 43) (var "y"))
                                       )
                                      )
                (apair
                 (apair (int 42) (apair (var "x") (aunit)))
                 (apair (apair (int 43) (apair (var "y") (aunit))) (aunit)))
                )

   (test-equal? "R list -> M list #3"
                (racketlist->mupllist (list (var "foo") (int 17)))
                (apair (var "foo") (apair (int 17) (aunit)))
                )


   (test-case "List tests"
              ; racketlist->mupllist
              (check-equal? (racketlist->mupllist (list (int 1) (int 2) (int 3) (int 4)))
                            (apair (int 1) (apair (int 2) (apair (int 3) (apair (int 4) (aunit))))))

              (check-equal? (racketlist->mupllist '())
                            (aunit) "empty racketlist")

              ; mupllist->racketlist
              (check-equal? (mupllist->racketlist (apair (int 1) (apair (int 2) (apair (int 3) (apair (int 4) (aunit))))))
                            (list (int 1) (int 2) (int 3) (int 4)) "simple mupllist")
              (check-equal? (mupllist->racketlist (aunit))
                            '() "empty mupllist")
              )
   )
  )

(define hw5-tests-part-B
  (test-suite
   "Tests for HW 5 Part B"

   (test-equal? "Simple int"
                (eval-exp (int 5))
                (int 5)
                )


   (test-equal? "Simple addition"
                (eval-exp (add (int 10) (int 7)))
                (int 17)
                )

   (test-equal? "Simple ifgreater"
                (eval-exp (ifgreater (int 10) (int 5) (int 1) (int 0)))
                (int 1)
                )

   (test-equal? "Simple ifgreater 2"
                (eval-exp (ifgreater (int 0) (int 5) (int 1) (int 0)))
                (int 0)
                )

   (test-equal? "Simple fst"
                (eval-exp (fst (apair (int 1) (int 2))))
                (int 1)
                )

   (test-equal? "Simple snd"
                (eval-exp (snd (apair (int 1) (int 2))))
                (int 2)
                )

   (test-equal? "Simple isaunit true"
                (eval-exp (isaunit (aunit)))
                (int 1)
                )

   (test-equal? "Simple isaunit false"
                (eval-exp (isaunit (int 10)))
                (int 0)
                )

   (test-equal? "basic function definition anonymous"
                (eval-exp (fun #f "x" (int 5)))
                (closure '() (fun #f "x" (int 5)))
                )

   (test-equal? "basic function definition 2"
                (eval-exp (fun "incr" "x" (add (var "x") (int 1))))
                (closure '() (fun "incr" "x" (add (var "x") (int 1))))
                )

   (test-equal? "scope"
                (eval-exp (mlet "x" (int 5) (mlet "x" (add (var "x") (int 1)) (var "x"))))
                (int 6))

   (test-equal? "basic-call 1"
                (eval-exp (call (fun #f "x" (int 1)) (aunit) ))
                (int 1)
                )

   (test-equal? "basic-call 2"
                (eval-exp (call (fun "incr" "x" (add (var "x") (int 1))) (int 42)))
                (int 43)
                )

   (test-equal? "basic scope with call"
                (eval-exp (call (mlet "x" (int 5)(fun "test" "x" (var "x"))) (int 100)))
                (int 100)
                )

   (test-equal? "basic scope with call 2"
                (eval-exp (call (mlet "x" (int 5)(fun "test" "x" (add (int 1)(var "x")))) (int 100)))
                (int 101)
                )

  (test-equal? "Local scoping"
               (eval-exp (mlet "f1"
                               (fun #f "a" (mlet "x"
                                                 (var "a")
                                                 (fun #f "z" (add (var "x") (int 1)))))
                               (mlet "f3"
                                     (fun #f "f"
                                          (mlet "x"
                                                (int 1729)
                                                (call (var "f") (aunit))))
                                     (call (var "f3") (call (var "f1") (int 1))))))
               (int 2)
               )

  (test-equal? "ifgreater with invalid e4"
               (eval-exp (ifgreater (add (int 2) (int 2)) (add (int 2) (int 1)) (add (int 3) (int -3)) (add "wrong" "bad")))
               (int 0)
               )

  (test-equal? "fst/snd test"
               (eval-exp (apair (fst (apair (int 1) (int 2)))
                                (snd (apair (int 3) (int 4))) ))
               (apair (int 1) (int 4))
               )

  (test-equal? "apair should reduce its contents"
               (eval-exp (apair (fst (apair  (int 3) (int 10))) (int 4)))
               (apair (int 3) (int 4)))


  (test-equal? "fst should reduce its expression before returning"
               (eval-exp (fst (apair (add (int 1) (int 2)) (int 3))))
               (int 3))

  (test-equal? "snd should reduce its expression before returning"
               (eval-exp (snd (apair (add (int 1) (int 2)) (int 4))))
               (int 4))

  (test-equal? "make sure isaunit evaluates its parameter"
               (eval-exp (isaunit (snd (apair (int 1) (aunit)))))
               (int 1))

  (test-equal? "test call of previously defined function"
               (eval-exp (mlet "test" (fun #f "b" (var "b")) (call (var "test") (int 15))))
               (int 15)
               )

  (test-equal? "simple mlet"
               (eval-exp (mlet "a" (int 5) (var "a")))
               (int 5)
               )

  (test-equal? "Sum over list: test of recursive function"
               (eval-exp (mlet "fnc"
                               (fun "f1" "x"
                                    (ifgreater (isaunit (var "x")) (int 0)
                                               (int 0)
                                               (add (fst (var "x")) (call (var "f1") (snd (var "x"))))))
                               (call (var "fnc") (apair (int 1) (aunit)))))
               (int 1)
               )

  (test-equal? "Sum over list: test of recursive function 2"
               (eval-exp (mlet "fnc"
                               (fun "f1" "x"
                                    (ifgreater (isaunit (var "x")) (int 0)
                                               (int 0)
                                               (add (fst (var "x")) (call (var "f1") (snd (var "x"))))))
                               (call (var "fnc") (apair (int 1) (apair (int 2) (apair (int 3) (aunit)))))))
               (int 6)
               )
   ; add
   (test-case "add"
              (check-equal?
               (eval-exp (add (int 3) (int 4)))
               (int 7) "simple add")
              (check-equal? (eval-exp (int 5)) (int 5) "int evaluation")
              (check-equal? (eval-exp (add (add (int 1) (int 2)) (add (int 3) (int 4)))) (int 10) "complex add")
              )

   ;(check-exn #rx"MUPL" (lambda () (eval-exp (add (int 3) (aunit)))) "add exception")

   (test-case "simple aunit"
              (check-equal? (eval-exp (aunit)) (aunit) "aunit evaluation")
              )

   (test-case "closure"
              (check-equal? (eval-exp (closure '() (fun #f "x" (var "x"))))
                            (closure '() (fun #f "x" (var "x"))) "closure evaluation")
              )

   ; mlet and var
   (test-case "mlet and var"
              (check-equal? (eval-exp (mlet "x" (add (int 1) (int 1)) (var "x"))) (int 2) "mlet and var 1")
              (check-equal? (eval-exp (mlet "x" (int 1) (var "x"))) (int 1) "mlet and var 2")
;              (check-exn #rx"unbound" (lambda () (eval-exp (var "x"))) "var exception")
              )

   (test-case "fun with let"
              (check-equal? (eval-exp (fun #f "x" (var "x")))
                            (closure '() (fun #f "x" (var "x"))) "fun evaluation")
              (check-equal? (eval-exp (mlet "x" (int 1) (fun #f "a" (var "x"))))
                            (closure (list (cons "x" (int 1))) (fun #f "a" (var "x"))) "mlet and fun evaluation")
              )
    ; ifgreater
   (test-case "simple ifgreater, false"
              (check-equal? (eval-exp (ifgreater (int 1) (int 2) (int 3) (int 4))) (int 4))
              )

   (test-case "complex ifgreater, false"
              (check-equal? (eval-exp (ifgreater (add (int 0)(int 1)) (add (int 0)(int 2)) (int 3) (int 4))) (int 4) ))

   (test-case "complex ifgreater, false 2"
              (check-equal? (eval-exp (ifgreater (int 1) (int 2) (int 3) (add (int 2)(int 2)))) (int 4))
              )

   (test-case "ifgreater"
              (check-equal? (eval-exp (ifgreater (int 2) (int 1) (int 3) (int 4))) (int 3)  "simple ifgreater, true")
              (check-equal? (eval-exp (ifgreater (add (int 0)(int 2)) (add (int 1) (int 0)) (int 3) (int 4))) (int 3) "complex ifgreater, true")
              (check-equal? (eval-exp (ifgreater (int 2) (int 1) (add (int 1)(int 2)) (int 4))) (int 3) "complex ifgreater, true 2")
 ;             (check-exn #rx"MUPL" (lambda () (eval-exp (ifgreater "1" (int 2) (int 3) (int 4)))) "ifgreater exception")
              )

   (test-case "apair"
              (check-equal? (eval-exp (apair (int 1) (int 1))) (apair (int 1) (int 1)) "int apair")
              (check-equal? (eval-exp (mlet "x" (int 1) (apair (var "x") (var "x"))))
                            (apair (int 1) (int 1)) "var apair")
              )

   (test-case "fst"
   ; fst
              (check-equal? (eval-exp (fst (apair (int 1) (int 2)))) (int 1) "simple fst")
              (check-equal? (eval-exp (mlet "x" (apair (int 1) (int 2)) (fst (var "x")))) (int 1) "mlet and fst")
;              (check-exn #rx"MUPL" (lambda () (eval-exp (fst (add (int 1) (int 2))))) "fst exception")
              )

   (test-case "snd"
   ; snd
              (check-equal? (eval-exp (snd (apair (int 1) (int 2)))) (int 2) "snd evaluation")
              (check-equal? (eval-exp (mlet "x" (apair (int 1) (int 2)) (snd (var "x")))) (int 2) "mlet and snd")
              ;(check-exn #rx"MUPL" (lambda () (eval-exp (snd (add (int 1) (int 2))))) "snd exception")
              )

   (test-case "isaunit"
   ; isaunit
              (check-equal? (eval-exp (isaunit (aunit))) (int 1) "simple isaunit true")
              (check-equal? (eval-exp (mlet "x" (aunit) (isaunit (var "x")))) (int 1) "mlet isaunit true")
              (check-equal? (eval-exp (isaunit (int 0))) (int 0) "simple isaunit false")
              (check-equal? (eval-exp (mlet "x" (int 0) (isaunit (var "x")))) (int 0) "mlet isaunit false")
              )

   (test-case "call"
   ; call
              (check-equal? (eval-exp (mlet "double" (fun "double" "x" (add (var "x") (var "x")))
                                            (call (var "double") (int 10))))
                            (int 20) "double function, non-recursive")
              (check-equal?
               (eval-exp
                (mlet "range"
                      (fun "range" "lo"
                           (fun #f "hi"
                                (ifgreater (var "lo") (var "hi") (aunit)
                                           (apair (var "lo") (call (call (var "range") (add (int 1) (var "lo"))) (var "hi"))))))
                      (call (call (var "range") (int 5)) (int 8))))
               (apair (int 5) (apair (int 6) (apair (int 7) (apair (int 8) (aunit))))) "range function, recursive")
              ;(check-exn #rx"MUPL" (lambda () (eval-exp (call (int 1) (int 2)))) "call exception")
              )


  )
  )

(define hw5-tests-part-C
  (test-suite
   "Tests for HW 5 Part C"

   (test-equal? "ifaunit test #1"
                (eval-exp (ifaunit (aunit) (int 2) (int 3)))
                (int 2)
                )

   (test-equal? "ifaunit test #2"
                (eval-exp (ifaunit (int 3) (int 2) (int 3)))
                (int 3)
                )
   ; ifaunit
   (test-case "ifaunit"
              (check-equal? (eval-exp (ifaunit (aunit) (add (int 1)(int 2)) (add (int 3)(int 4)))) (int 3) "ifaunit true")
              (check-equal? (eval-exp (ifaunit (int 0) (add (int 1)(int 2)) (add (int 3)(int 4)))) (int 7) "ifaunit false")
              )


   (test-case "mlet*"
   ; mlet*
              (check-equal? (eval-exp (mlet* (cons (cons "x" (int 1)) null) (var "x")))
                            (int 1) "mlet* basic test")
              (check-equal? (eval-exp (mlet* (list (cons "f" (int 2)) (cons "y" (int 15))) (add (var "f") (add (var "y") (int 3)))))
                            (int 20) "mlet a bit more complicated")
              (check-equal? (eval-exp (mlet* (list (cons "x" (int 1)) (cons "y" (int 2))) (add (var "x")(var "y"))))
                            (int 3) "normal mlet* evaluation")
              (check-equal? (eval-exp (mlet* (list (cons "x" (int 1))) (var "x")))
                            (int 1) "single variable mlet* evaluation")
              (check-equal? (eval-exp (mlet* (list (cons "x" (int 1)) (cons "x" (int 2))) (var "x")))
                            (int 2) "shadowing mlet* evaluation")
              )

   (test-case "ifeq"
   ; ifeq
              (check-equal? (eval-exp (ifeq (int 1) (int 1) (int 2) (int 3)))
                            (int 2) "simple ifeq true evaluation")
              (check-equal? (eval-exp (ifeq (int 0) (int 1) (int 2) (int 3)))
                            (int 3) "simple ifeq false evaluation")
              (check-equal? (eval-exp (ifeq (add (int 1)(int 1)) (int 2) (int 2) (int 3)))
                            (int 2) "complex ifeq true evaluation")
              (check-equal? (eval-exp (ifeq (add (int 1)(int 1)) (int 1) (int 2) (int 3)))
                            (int 3) "complex ifeq false evaluation")
              )


   ; else
   ;(check-exn #rx"MUPL" (lambda () (eval-exp (list (int 1) (int 2)))) "bad expression exception")

   )
  )

(define hw5-tests-part-D
  (test-suite
   "Tests for HW 5 Part D"


   (test-case "ifeq"
              (check-equal?
               (int 1)
               (eval-exp (ifeq (int 2) (add (int 1) (int 1)) (int 1) (int 2)))
               )

              (check-equal?
               (eval-exp (ifeq (int 2) (add (int 1) (int 2)) (int 1) (int 2)))
               (int 2)
               )
              )

   (test-case "mupl-map"
              (define addtwo (fun "addone" "x" (add (var "x") (int 2))))
              (define mupl-map-addtwo (call mupl-map addtwo))
              (check-equal? (eval-exp (call mupl-map-addtwo (aunit))) (aunit))

              (define my-mupl-list (apair (int 23) (apair (int 42) (aunit))))
              (define my-answers (apair (int 25) (apair (int 44) (aunit))))
              (check-equal? (eval-exp (call mupl-map-addtwo my-mupl-list)) my-answers))

   (test-case "mupl-map"
              ; mupl-map
              (check-equal? (eval-exp
                             (call (call mupl-map (fun #f "x" (add (int 1) (var "x"))))
                                   (apair (int 1) (apair (int 2) (aunit)))))
                            (apair (int 2) (apair (int 3) (aunit))) "map normal list")
              (check-equal? (eval-exp
                             (call (call mupl-map (fun #f "x" (add (int 1) (var "x"))))
                                   (apair (int 1) (aunit))))
                            (apair (int 2) (aunit)) "map single item list")
              (check-equal? (eval-exp
                             (call (call mupl-map (fun #f "x" (add (int 1) (var "x"))))
                                   (aunit)))
                            (aunit) "map empty list")
              )

   (test-case "mupl-mapAddN"
              (define input (apair (int 25) (apair (int 44) (aunit))))
              (define output (apair (int 26) (apair (int 45) (aunit))))
              (check-equal? (eval-exp (call (call mupl-mapAddN (int 1)) input)) output))

   (test-case "mupl-mapAddN 2"
              (check-equal? (eval-exp (call (call mupl-mapAddN (int 7))
                                            (racketlist->mupllist '())))
                            (aunit) "mapAddN empty list")
              )

   (test-case "mupl-mapAddN 3"
              (check-equal? (eval-exp (call (call mupl-mapAddN (int 7))
                                            (racketlist->mupllist (list (int 3) (int 4) (int 9)))))
                            (racketlist->mupllist (list (int 10) (int 11) (int 16))) "mapAddN +7")
              )

   (test-case "mupl-mapAddN 4"
              (check-equal? (eval-exp (call (call mupl-mapAddN (int 7))
                                            (racketlist->mupllist (list (int 3)))))
                            (racketlist->mupllist (list (int 10))) "mapAddN single item list")
              )


   ))



(run-tests hw5-tests-part-A)
(run-tests hw5-tests-part-B)
(run-tests hw5-tests-part-C)
(run-tests hw5-tests-part-D)
