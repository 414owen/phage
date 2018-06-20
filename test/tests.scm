(import "prelude/prelude.scm")
(import "stdlib/comp.scm")

(def tlst (list 1 2 3 4))

(putsl "creating test structure")

(import "test/eq.scm")

(putsl "getting length of cases")

(def cases (len tests))

(putsl "running tests")

(fn run (tests)
	(fn runrec (n a test)
		(printl n)
		(if (= test ())
			a
			(runrec
				(+ 1 n)
				(+ a (if (= (eval (caar test)) (eval (cadar test)))
					1
					(do
					  (printl (car test))
					  (printl (eval (caar test)))
					  (printl (eval (cadar test)))
					  0)
				))
				(cdr test))))
	(runrec 0 0 tests))

(def passed (run tests))
(puts "cases: ")  (printl cases)
(puts "passed: ") (printl passed)
(puts "failed: ") (printl (- cases passed))
