(import "prelude/prelude.scm")

(def tlst (list 1 2 3 4))

(print "creating test structure")

(import "test/eq.scm")

(print "getting length of cases")

(def cases (len tests))

(print "running tests")

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
(print)
(print "cases:" cases)
(print "passed:" passed)
(print "failed:" (- cases passed))
