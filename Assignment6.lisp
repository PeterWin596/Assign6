; Assignment6.lisp
; All functions and macros defined as per Assignment 6 instructions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1. myList: Returns a specific nested list
(defun myList ()
  (list 4 (list 7 22) "art" (list "math" (list 8) 99) 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2. leapYear: Returns a list of all leap years from 1800 through 2025
(defun leapYear ()
  (labels ((is-leap (y)
             (or (and (zerop (mod y 4)) (not (zerop (mod y 100))))
                 (zerop (mod y 400)))))
    (labels ((collect (year acc)
               (if (> year 2025)
                   (reverse acc)
                   (collect (1+ year)
                            (if (is-leap year)
                                (cons year acc)
                                acc)))))
      (collect 1800 '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 3. union-: Merges two lists into one without duplicates (non-destructive)
(defun union- (list1 list2)
  (labels ((contains (item lst)
             (cond ((null lst) nil)
                   ((equal item (car lst)) t)
                   (t (contains item (cdr lst)))))
           (append-unique (src acc)
             (if (null src)
                 acc
                 (let ((head (car src)))
                   (append-unique (cdr src)
                                  (if (contains head acc)
                                      acc
                                      (cons head acc)))))))
    (append-unique list2 list1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 4. avg: Returns the average of a list of numbers, or NIL if list is empty
(defun avg (aList)
  (labels ((tail-avg (lst sum count)
             (if (null lst)
                 (if (= count 0) nil (/ sum count))
                 (tail-avg (cdr lst)
                           (+ sum (car lst))
                           (+ count 1)))))
    (tail-avg aList 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 5. isType: Returns a function that checks if input is of a given type
(defun isType (dataType)
  (lambda (x) (typep x dataType)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 6. taxCalculator: Applies rate to numbers in list above limit (bonus: tail-recursive)
(defun taxCalculator (limit rate values)
  (labels ((tail-tax (vals acc)
             (if (null vals)
                 (reverse acc)
                 (let ((v (car vals)))
                   (tail-tax (cdr vals)
                             (cons (if (> v limit) (* v rate) v) acc))))))
    (tail-tax values '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 7. clean: Filters a list (and sublists) using a predicate function
(defun clean (aFunc aList)
  (cond
    ((null aList) nil)
    ((listp (car aList))
     (cons (clean aFunc (car aList))
           (clean aFunc (cdr aList))))
    ((funcall aFunc (car aList))
     (cons (car aList)
           (clean aFunc (cdr aList))))
    (t (clean aFunc (cdr aList)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 8. threeWayBranch: Custom macro simulating a 3-way conditional branch
(defmacro threeWayBranch (x y z)
  `(cond
     ((eval ,(car x)) ,@(cdr x))
     ((eval ,(car y)) ,@(cdr y))
     ((eval ,(car z)) ,@(cdr z))))
