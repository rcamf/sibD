#lang racket
(require pict)
(require pict/tree-layout)

(define tree0 '((3) ((1 2)) ((4 5))))

(define tree1 '((9 21 31)
               ((1 2))
               ((10 12 17 19))
               ((32 35 40))))

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

(define (split-child child m)
  (define content (first child))
  (define children (rest child))
  (let*-values ([(left-content other-content) (split-at content m)]
                [(new-root) (first other-content)]
                [(right-content) (rest other-content)]
                [(left-children right-children) (if (empty? children)
                                                    (values '() '())
                                                    (split-at children (+ 1 m)))]
                )
    (values new-root (list (list* left-content left-children)
                           (list* right-content right-children)))))

(define (split content m left target-child right index)
  (if (<= (length (first target-child))  (* 2 m))
    (append (list content) left (list target-child) right)
    (let*-values ([(divider trees) (split-child target-child m)]
                  [(left-content right-content) (split-at content index)])
      (list* (append left-content (list divider) right-content)
            (append left trees right)))))

(define (bbaum-insert-rec tree m element)
  (define content (first tree))
  (define children (rest tree))
  (if (= 1 (length tree))
      (list (sort (cons element content) <))
      (let*-values ([(res) (index-where content (lambda (x) (> x element)))]
                    [(index) (if (equal? #f res) (length content) res)]
                    [(left others) (split-at children index)]
                    [(target-child) (first others)]
                    [(right) (rest others)])
        (split content m left (bbaum-insert-rec target-child m element) right index))))

(define (bbaum-insert tree m element)
  (let ([new-tree (bbaum-insert-rec tree m element)])
    (if (>= (* 2 m) (length (first new-tree)))
        new-tree
        (let-values ([(root children) (split-child new-tree m)])
          (list* (list root) children)))))

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

(define (insert-all tree m elements)
  (foldl (lambda (e t)
           (begin
             (show-pict (render t))
           (bbaum-insert t m e))) tree elements))