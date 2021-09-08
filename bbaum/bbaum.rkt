#lang racket
(require pict)
(require pict/tree-layout)

(define tree '((9 21 31)
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

(define (render-node content)
  (frame (inset
          (text (string-join (map number->string content)))
          10)))

(define (b-baum tree)
  (apply tree-layout
         (map b-baum (rest tree))
         #:pict (render-node (first tree)) ))

(naive-layered (b-baum tree2))