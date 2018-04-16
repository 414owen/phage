(fun dot (a b c) (a (b c)))
(fun const (a b) a)

(fun fold (zero fn lst)
	(fun foldrec (acc lst)
		(if (= lst ()) acc
			(foldrec (fn (car lst) acc) (cdr lst))))
	(foldrec zero lst))

(def rev (fold () cons))

(fun map (fn lst)
	 (rev (fold ()
			(\(el acc) (cons (fn el) acc)) lst)))

(def sum (fold 0 +))
(def prod (fold 1 *))
(def len (dot sum (map (const 1))))
(fun filter (fn lst)
	(rev (fold () (\(el acc)
		(if (fn el) (cons el acc) acc)) lst)))

(fun range (start end step)
	(def fin (if (> step 0) >= <=))
	(if (fin start end) ()
		(cons start (range (+ start step) end step))))

(fun append (a b)
	 (if (= a ()) b (cons (car a) (append (cdr a) b))))

(fun concat (lsts)
	(if (= lsts ()) ()
		(append (car lsts) (concat (cdr lsts)))))

(fun intersperse (el lst)
	(fun insp (el lst)
		(if (= lst ()) ()
			(cons el (cons (car lst) (insp el (cdr lst))))))
	(cdr (insp el lst)))

(fun intercalate (lsta lsts)
	(concat (intersperse lsta lsts)))
