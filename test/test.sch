(fun fact (a) (if (<= a 0) 1 (* a (fact (- a 1)))))

(def tests
	(quote (
		((+ 1 2) 3)
		((* 3 3) 9)
		((/ 10 4) 2)
		((% 10 3) 1)
		((min 3 4) 3)
		((max 3 4) 4)
		((= 2 2) true)
		((= 2 4) false)
		((< 3 4) true)
		((> 3 4) false)
		((<= 3 3) true)
		((>= 3 4) false)
		((list 1 2 3) (quote (1 2 3)))
		((list 1 2 3 (list 4 5) 6 7) (quote (1 2 3 (4 5) 6 7)))
		((cons 1 ()) (quote (1)))
		((cons 3 (list 4 5 6)) (quote (3 4 5 6)))
		((car (list 3 4 5)) 3)
		((cdr (list 3 4 5 6)) (quote (4 5 6)))
		((cadadr (list (list 2 (list 4 5 6)))) (quote (5 6)))
	)))

(fun len (lst)
	(fun lrec (acc l)
		(if (= l ())
			acc
			(lrec (+ acc 1) (cdr l))))
	(lrec 0 lst))

(def cases (len tests))

(fun run (t)
	(fun runrec (a t)
		(if (= t ())
			a
			(runrec
				(+ a (if (= (eval (caar t)) (eval (cadar t)))
					1
					(do (print (car t)) 0)
				))
				(cdr t))))
	(runrec 0 t))

(def passed (run tests))
(print)
(print (quote cases:) cases)
(print (quote passed:) passed)
(print (quote failed:) (- cases passed))
