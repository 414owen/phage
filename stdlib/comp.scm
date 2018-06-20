(import "prelude/prelude.scm")

/*

List comprehensions for Phage

format: (comp <list-exp1> <var1> <list-exp2> <var2> ... <result-exp>)

examples

λ: (comp (list 1 2 3) x x)
>> (1 2 3)
λ: (comp (list 1 2 3) x (list 4 5 6) y (* x y))
>> (4 5 6 8 10 12 12 15 18)

*/

(fm comp (lst var exp)
	(dfn layer (_exp)
		(def _args args)
		(if (= rest ())
			(list (eval _exp))
			(concat
				(map
					((\(_binding)
						(\(_el)
							(call def _binding _el)
							(apply layer (cddr _args)))) (car rest))
					(eval _exp)))))
	(apply layer args))
