;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Problems 1-10|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;#1 a tip of 15% has been added to the cost
(define (amt-due price)
  (* price 1.15))

;#2 sum of a geometric progression
(define (geom-sum a r n)
  (*(/(-(expt r n)1)(- r 1))a))

;#3 returns the fee that depends on the age
(define (fee age)
  (cond
    ((or(<= age 3)(>= age 65)) 0)
    ((and(<= 4 age)(<= age 6)) 0.5)
    ((and(<= 7 age)(<= age 12))  1.0)
    ((and(<= 13 age)(<= age 15)) 1.5)
    ((and(<= 16 age)(<= age 18)) 1.8)
    (else 2.0)
  )
)
  
;#4 returns the number of even values that are in the list
(define (evens-count myList)
  (if(empty? myList) 0
     (if(even? (car myList)) (+(evens-count(cdr myList))1)
           (evens-count(cdr myList)))))

;#5 returns the list with all of the even integers removed from the list
(define (remove-evens myList)
  (if(empty? myList) (list) ;kara no list
      (if(even? (car myList)) (remove-evens (cdr myList))
         (cons (car myList) (remove-evens (cdr myList)))))) ;kisuu ga car list ni nokotte iru

;#6 returns a slope
(define (slope x1 x2 y1 y2)
  (/(- y1 y2) (- x1 x2)))

;#7 returns a distance
(define (distance x1 x2 y1 y2)
  (let ((a(expt(- x1 x2) 2)))
    (let ((b(expt(- y1 y2) 2)))
      (sqrt (+ a b)))))

;#8 returns a grade that depends on the score
(define (grade score)
   (cond
    ((< score 59.5) "F")
    ((and(>= score 59.5)(< score 69.5)) "D")
    ((and(>= score 69.5)(< score 79.5)) "C")
    ((and(>= score 79.5)(< score 89.5)) "B")
    (else "A"))
  )

;#9 returns the given list in the reverse order
(define (reverce-list myList)
  (if(empty? (cdr myList)) (list(car myList)) ;syuuten suuji ga kaeru
         (append (reverce-list (cdr myList)) (list (car myList)))))
  
  
;#10 returns the last item of a given list
(define (last-item myList)
  (if(empty? (cdr myList)) (car myList) ;car ni surukotode list dehanaku number ga kaeru  ;mylist dake dato list ga kaeru
     (last-item (cdr myList))))


;Test
(define (f x)
  (+ x 3))

(define (g y)
  (* y 2))

(define (h myList)
  (if (null? myList) 10
      (* (car myList)(h(cdr myList)))))

(define (sum-evens myList)
  (if (empty? myList) 0
      (if (even? (car myList)) (+(car myList) (sum-evens (cdr myList)))
          (sum-evens (cdr myList))))
)
