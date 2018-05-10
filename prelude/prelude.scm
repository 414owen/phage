(fun fold (zero fn lst)
	((\(acc lst)
		(if (= lst ()) acc
			(rec (fn (car lst) acc) (cdr lst))))
		zero lst))

// homogeneous binary function helper

(fun homBinFunc (fn) (\(a b) (fold b fn (cons a rest))))


// boolean logic

(def &  (homBinFunc (\(a b) (! (| (! a) (! b))))))
(def -> (homBinFunc (\(a b) (| (! a) b))))
(def ^  (homBinFunc (\(a b) (& (| a b) (! (& a b))))))
(def ~= (homBinFunc (\(a b) (! (^ a b)))))

(let
	(implies ->)
	(and &)
	(xor |)
	(equiv ~=))


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

(fun choose (fn) (homBinFunc (\(a b) (if (fn a b) a b))))
(def min (choose <))
(fun minl (l) (fold (car l) min l))
(def max (choose >))
(fun maxl (l) (fold (car l) max l))
(def rev (fold () cons))
(def last (dot car rev))
(def init (dot rev (dot cdr rev)))
(def all (fold true &))
(def any (fold false |))
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

(fun caror (el lst) (if (= lst ()) el (car lst)))

(fun range (start end)
	(def step (caror 1 rest))
	(def fin (if (> step 0) >= <=))
	(if (fin start end) ()
		(cons start (range (+ start step) end step))))

(def upto (pipe (+ 1) (range 1)))

(fun append (a b)
	 (if (= a ()) b (cons (car a) (append (cdr a) b))))

(fun concat (lsts)
	(if (= lsts ()) ()
		(append (car lsts) (concat (cdr lsts)))))

(fun intersperse (el lst)
	(cdr ((\(el lst)
		(if (= lst ()) ()
			(cons el (cons (car lst) (rec el (cdr lst))))))
		 el lst)))

(def intercalate (pipe intersperse concat))
