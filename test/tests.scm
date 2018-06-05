(import "prelude/prelude.scm")

(def tlst (list 1 2 3 4))

(puts "creating test structure")

(import "test/eq.scm")

(puts "getting length of cases")

(def cases (len tests))

(puts "running tests")

(fn run (tests)
	(fn runrec (n a test)
		(print n)
		(if (= test ())
			a
			(runrec
				(+ 1 n)
				(+ a (if (= (eval (caar test)) (eval (cadar test)))
					1
					(do
					  (print (car test))
					  (print (eval (caar test)))
					  (print (eval (cadar test)))
					  0)
				))
				(cdr test))))
	(runrec 0 0 tests))

(def passed (run tests))
(puts "cases: ") (print cases)
(puts "passed: ") (print passed)
(puts "failed: ") (print (- cases passed))
