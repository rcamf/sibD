#lang racket
(require pict)
(require pict/tree-layout)

(define tree0 '((3) ((1 2)) ((4 5))))

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

; Conversion:
(define (node->bbaum t)
  (define content (append (first t) '(+inf.0)))
  (define children (rest t))
  (if (empty? children)
      (map list content)
      (map cons content (map node->bbaum children))))

(define (bbaum->node tree)
  (if (empty? tree)
      empty
      (list* (drop-right (map car tree) 1)
	 (filter-not empty? (map (compose bbaum->node cdr) tree)))))


; Logic:
(define (split m target)
  (define child (cdr target))
  (if (number? child)
      (list (list child) (list (car target)))
      (if (<= (length child) (+ 1 (* 2 m)))
	  (list target)
	  (let*-values ([(left others) (split-at child m)]
			[(middle right) (split-at others 1)])
	    (list (cons (caar middle) (append left (list (cons +inf.0 (cdr middle)))))
		  (cons (car target) right))))))

(define (bbaum-insert-rec tree m element)
  (if (null? tree)
      element
      (let*-values ([(left others) (splitf-at tree
					      (lambda (entry) (> element (first entry))))]
		    [(target right) (split-at others 1)]
		    [(divider) (caar target)]
		    [(new-child) (bbaum-insert-rec (cdar target) m element)])
	(append left (split m (cons divider new-child)) right))))

(define (bbaum-insert tree m element)
  (let ([new-tree (bbaum-insert-rec tree m element)])
    (if (<= (length new-tree) (+ 1 (* 2 m)))
	new-tree
	(split m (cons +inf.0 new-tree)))))


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

; (define (insert-all tree m elements)
;   (foldl (lambda (e t)
;            (begin
;              (show-pict (render t))
;            (bbaum-insert t m e))) tree elements))


(define b1 (node->bbaum tree3))
(render-bbaum b1)
(define b2 (bbaum-insert b1 2 9))
(render-bbaum b2)
(define b3 (bbaum-insert b2 2 14))
(render-bbaum b3)
(define b4 (bbaum-insert b3 2 8))
(render-bbaum b4)
