; BINOMIAL QUEUE

; supported operations:
;    * findMin(q)   - return minimum element of queue q
;    * insert(e, q) - insert element e into queue q
;    * meld(q1, q2) - merge queues q1 and q2 into a single queue
;    * deleteMin(q) - remove minimum element of queue q


#|

Binomial Tree Representation
    - implemented as a list of list
    - a tree's children are maintained in decreasing order of rank

(0)                                       0          (rank 0)

(0 (1))                                   0          (rank 1)
                                          |
                                          1

(0 (2 (3)) (1))                           0          (rank 2)
                                         / \
                                        2   1
                                        |
                                        3

(0 (4 (6 (7)) (5)) (2 (3)) (1))           0          (rank 3)
                                         /|\
                                        / | \
                                       4  2  1
                                      /|  |
                                     6 5  3
                                     |
                                     7

|#


; return root of tree
(define root
  (lambda (tree)
    (car tree)))

; return rank of tree (number of children)
(define rank
  (lambda (tree)
    (define helper
      (lambda (children)
        (cond ((null? children) 0)
              (else (+ 1 (helper (cdr children)))))))
    (helper (cdr tree))))

; link two trees of equal rank
(define link
  (lambda (tree1 tree2)
    (cond ((null? tree1) tree2)
          ((null? tree2) tree1)
          ((< (root tree1) (root tree2)) (cons (root tree1) (cons tree2 (cdr tree1))))
          (else (cons (root tree2) (cons tree1 (cdr tree2)))))))




#|

Binomial Queue Representation
    - implemented as a list of trees
    - a queue's trees are maintained in increasing order of rank
    - the structure of a queue resembles the binary representation of the number of elements
      it contains (in reversed order)

(() (0 (1)) () (4 (8 (10 (11)) (9)) (6 (7)) (5))

  |                                |      -->   queue containing 10 elements
  |  ()    0     ()          4     |
  |        |                /|\    |
  |        1               / | \   |
  |                       8  6  5  |
  |                      /\  |     |
  |                     10 9 7     |
  |                     |          |
  |                     11         |
  |                                |

|#


; return minimum element of queue
(define findMin
  (lambda (queue)
    (define helper
      (lambda (minimum queue)
        (cond ((null? queue) minimum)
              ((null? (car queue)) (helper minimum (cdr queue)))
              (else (helper (min minimum (root (car queue))) (cdr queue))))))
    (cond ((null? (car queue)) (findMin (cdr queue)))
          (else (helper (root (car queue)) (cdr queue))))))

; insert new element into queue
(define insert
  (lambda (element queue)
    (meld (list (list element)) queue)))

; merge/meld two queues
(define meld
  (lambda (queue1 queue2)
    (define helper
      (lambda (carry queue1 queue2)
        (cond ((null? queue1) (cond ((null? carry) '())
                                    (else (cons carry '()))))
              ((and (null? (car queue1)) (null? (car queue2))) (cons carry (helper '() (cdr queue1) (cdr queue2))))
              ((or (null? (car queue1)) (null? (car queue2))) (cond ((null? carry) (cons (link (car queue1) (car queue2)) (helper '() (cdr queue1) (cdr queue2))))
                                                                    (else (cons '() (helper (link carry (link (car queue1) (car queue2))) (cdr queue1) (cdr queue2))))))
              (else (cons carry (helper (link (car queue1) (car queue2)) (cdr queue1) (cdr queue2)))))))
    (cond ((< (qlength queue1) (qlength queue2)) (helper '() (extend (- (qlength queue2) (length queue1)) queue1) queue2))
          (else (helper '() queue1 (extend (- (qlength queue1) (qlength queue2)) queue2))))))

; delete minimum element from queue
(define deleteMin
  (lambda (queue)
    (define helper
      (lambda (minimum queue)
        (cond ((null? (car queue)) (cons '() (helper minimum (cdr queue))))
              ((= (root (car queue)) minimum) (cons '() (cdr queue)))
              (else (cons (car queue) (helper minimum (cdr queue)))))))
    (cond ((= (rank (find (findMin queue) queue)) 0) (helper (findMin queue) queue))
          (else (meld (helper (findMin queue) queue) (reverseq (cdr (find (findMin queue) queue))))))))

; append n empty trees in front of queue (positive sign extension)
(define extend
  (lambda (n queue)
    (define helper
      (lambda (n)
        (cond ((zero? n) '())
              (else (cons '() (helper (- n 1)))))))
    (append queue (helper n))))

; return length of queue (number of trees it contains)
(define qlength
  (lambda (queue)
    (cond ((null? queue) 0)
          (else (+ 1 (qlength (cdr queue)))))))

; find tree with root n
(define find
  (lambda (n queue)
    (cond ((null? (car queue)) (find n (cdr queue)))
          ((= (root (car queue)) n) (car queue))
          (else (find n (cdr queue))))))

; reverse queue
(define reverseq
  (lambda (queue)
    (reverse queue)))
