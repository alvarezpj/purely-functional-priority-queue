; Binomial Queue

; supported operations:
;    * findMin(q)   - return minimum element of queue q
;    * insert(e, q) - insert element e into queue q
;    * meld(q1, q2) - merge queues q1 and q2 into a single queue
;    * deleteMin(q) - remove minimum element of queue q


#|

Binomial Tree Representation
    - children maintained in decreasing order of rank (to support link operation efficiently)

(a)                                       a (rank 0)

(a (b))                                   a (rank 1)
                                          |
                                          b

(a (c (d)) (b))                           a (rank 2)
                                         / \
                                        c   b
                                        |
                                        d

(a (e (g (h)) (f)) (c (d)) (b))           a (rank 3)
                                         /|\
                                        / | \
                                       e  c  b
                                      /|  |
                                     g f  d
                                     |
                                     h

|#

; *may have to change representation of empty tree*

; tree node definition
; defined a (key, data) pair
(define Node
  (lambda (key data)
    (list key data)))

; retrieve key of node
(define getKey
  (lambda (node)
    (car node)))

; retrieve data of node
(define getData
  (lambda (node)
    (car (cdr node))))

; binomial tree definition
; defined as list of tree nodes
(define BinomialTree '())

; NOTE: ALL OPERATIONS ASSUME TREES ARE NOT EMPTY; THAT IS, THEY CONTAIN AT LEAST ONE NODE

; return root of tree in not empty
(define root
  (lambda (tree)
    (car tree)))

; return rank of binomial tree (number of children)
; note: root node does not count
(define rank
  (lambda (tree)
    (define helper
      (lambda (children)
        (cond ((null? children) 0)
              (else (+ 1 (helper (cdr children)))))))
    (helper (cdr tree))))

; check whether two trees have equal rank
(define equalRank?
  (lambda (tree1 tree2)
    (= (rank tree1) (rank tree2))))

; link two trees of equal rank
(define link
  (lambda (tree1 tree2)
    (cond ((< (root tree1) (root tree2)) (cons (root tree1) (cons tree2 (cdr tree1))))
          (else (cons (root tree2) (cons tree1 (cdr tree2)))))))


#|

Binomial Queue Representation
    - list of trees
    - trees maintained in increasing order of rank (to support insert operation efficiently)

(() (a (b)) () (c (g (i (j)) (h)) (e (f)) (d)) --> queue containing 10 nodes

  |                                |
  |  ()    a     ()          c     |
  |        |                /|\    |
  |        b               / | \   |
  |                       g  e  d  |
  |                      /|  |     |
  |                     i h  f     |
  |                     |          |
  |                     j          |
  |                                |

|#

; NOTE: ALL OPERATIONS ASSUME QUEUES ARE NOT EMPTY; THAT IS, THEY CONTAIN AT LEAST ONE TREE

; return minimum element of queue
(define findMin
  (lambda (queue)
    (define helper
      (lambda (minimum queue)
        (cond ((null? queue) minimum)
              (else (helper (min minimum (root (car queue))) (cdr queue))))))
    (helper (root (car queue)) (cdr queue))))

; insert new element into queue
(define insert
  (lambda (element queue)
    (define helper
      (lambda (carry partial-sum queue)
        (cond ((null? queue) (append partial-sum (cons carry queue)))
              ((null? (car queue)) (append partial-sum (cons carry (cdr queue))))
              (else (helper (link carry (car queue)) (cons '() partial-sum) (cdr queue))))))
    (helper (list element) '() queue)))
