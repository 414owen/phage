(fun rev (lst)
	(fun revrec (lst acc)
		(if (= lst ()) acc
			(revrec (cdr lst) (cons (car lst) acc))))
	(revrec lst ()))

(fun map (lst fn)
	(fun maprec (lst fn acc)
		(if (= lst ())
			(rev acc)
			(maprec (cdr lst) fn (cons (fn (car lst)) acc))))
	 (maprec lst fn ()))

// (print (map (list 1 2 3 4 5) (\(a) (* a a))))
