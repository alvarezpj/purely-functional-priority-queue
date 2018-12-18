; APPLICATION: sorting a list of numbers in ascending order


(load "./binomial.scm")

; insert list of numbers into queue
(define insert-into-queue
  (lambda (list-of-numbers queue)
    (cond ((null? list-of-numbers) queue)
          (else (insert-into-queue (cdr list-of-numbers) (insert (car list-of-numbers) queue))))))

; sort list of numbers
(define sort
  (lambda (size queue)
    (cond ((zero? size) '())
          (else (cons (findMin queue) (sort (- size 1) (deleteMin queue)))))))


(define unordered-list  '(9 8 12 6 13 7 43 2 0 1 8))
(sort (length unordered-list) (insert-into-queue unordered-list '()))
