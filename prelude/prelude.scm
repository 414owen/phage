// returns a list of its arguments
(fun list () args)

// functional
(fun dot (a b c) (a (b c)))
(fun const (a b) a)

(fun pipe ()
	(def funs args)
	(fun piperec (params lst)
		(if (= lst ()) (car params)
		(rec (list (apply (car lst) params)) (cdr lst))))
	(\() (piperec args funs)))

(fun fold (zero fn lst)
	(fun foldrec (acc lst)
		(if (= lst ()) acc
			(foldrec (fn (car lst) acc) (cdr lst))))
	(foldrec zero lst))

(def rev (fold () cons))
(def last (dot car rev))
(def init (dot rev (dot cdr rev)))
(def all (fold true and))
(def any (fold false or))
(def rpipe (pipe list rev (apply pipe)))
(def dot rpipe)

// do just returns its last parameter
(fun do () (last args))

(fun map (fn lst)
	 (rev (fold ()
			(\(el acc) (cons (fn el) acc)) lst)))

(def sum (fold 0 +))
(def prod (fold 1 *))
(def len (pipe (map (const 1)) sum))
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
