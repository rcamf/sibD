#lang racket
(require pict)
(require pict/tree-layout)

(define tree0 '((3 8) ((1 2)) ((4 5)) ((6 7))))

(define tree1 '((3 5 7 9)))

(define tree2 '((24)
		((14 20)
		 ((1))
		 ((15))
		 ((21 23)))
		((28)
		 ((25 27))
		 ((30)))))

(define tree3 '((10 31 44)
		((2 3 5))
		((11 15 20 22))
		((35 40))
		((45 51))))

(struct entry (key child)
  #:transparent)

(define (make-leaf-entry x)
  (entry x empty))

(define (make-bbaum keys subtrees)
  (map entry
       (append keys '(+inf.0))
       subtrees))

(define (node->bbaum n)
  (match n
    [(list leaf-keys) (map make-leaf-entry (append leaf-keys '(+inf.0)))]
    [(list keys children ...) (make-bbaum keys (map node->bbaum children))]))

(define/match (bbaum->node tree)
  [((list (entry keys (list)) ... (entry +inf.0 (list)))) (list keys)]
  [((list (entry keys children) ...)) (list* (drop-right keys 1)
					     (map bbaum->node children))])

; Utility
(define (splitf-around lst proc)
  (let*-values ([(left others) (splitf-at lst (compose not proc))])
    (values left (first others) (rest others))))

(define (split-around lst pos)
  (let*-values ([(left others) (split-at lst pos)])
    (values left (first others) (rest others))))


; Insertion
(define/match (split e m)
  [((entry key child) m)
   (if (> (length child) (+ 1 (* 2 m)))
     (match-let-values ([(pred (entry new-key new-pred-child) succ) (split-around child m)])
       (list (entry new-key
		    (append pred (list (entry +inf.0 new-pred-child))))
	     (entry key succ)))
     (list e))])

(define (bbaum-insert-rec tree m element)
  (match tree
    [(list (entry keys '()) ...) (map make-leaf-entry (sort (cons element keys) <))]
    [entries (match-let-values ([(pred (entry target-key target-child) succ)
				 (splitf-around entries
						(lambda (e) (<= element (entry-key e))))])
	       (append pred
		       (split (entry target-key
				     (bbaum-insert-rec target-child m element)) m)
		       succ))] ))

(define (bbaum-insert tree m element)
  (match (split (entry +inf.0 (bbaum-insert-rec tree m element)) m)
    [(list (entry +.inf.0 child)) child]
    [x x]))


; Rendering:
(define (render-node content)
  (frame (inset
	   (text (string-join (map number->string content)))
	   10)))

(define (bbaum tree)
  (apply tree-layout
	 (map bbaum (rest tree))
	 #:pict (render-node (first tree)) ))

(define (render tree)
  (naive-layered (bbaum tree)))

(define (render-bbaum tree)
  (render (bbaum->node tree)))


(define b0 (node->bbaum tree0))
(define b1 (node->bbaum tree1))
(define b2 (node->bbaum tree2))
(define b3 (node->bbaum tree3))
