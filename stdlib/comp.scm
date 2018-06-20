(import "prelude/prelude.scm")

/*

format: (comp <list-exp1> <var1> <list-exp2> <var2> ... <result-exp>)
example: (comp (list 1 2 3) x x)

*/

(fm comp (lst var exp)
	(dfn layer (_exp)
		(def _args args)
		(if (= rest ())
			// we're at the result expression
			(list (eval _exp))
			(concat
				(map
					((\(_binding)
						(\(_el)
							(call def _binding _el)
							(apply layer (cddr _args)))) (car rest))
					(eval _exp)))))
	(apply layer args))
