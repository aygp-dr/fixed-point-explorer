;; Length


;; [[file:y-combinator.org::*Length][Length:1]]
(defun length-maker (f)
  "Creates a length function using the given recursion mechanism."
  (lambda (lst)
    (if (null lst)
        0
      (+ 1 (funcall f (cdr lst))))))

(defvar length-y (Y #'length-maker)
  "Length function created with Y combinator")
;; Length:1 ends here

;; Append


;; [[file:y-combinator.org::*Append][Append:1]]
(defun append-maker (f)
  "Creates an append function using the given recursion mechanism."
  (lambda (lst1 lst2)
    (if (null lst1)
        lst2
      (cons (car lst1) (funcall f (cdr lst1) lst2)))))

(defvar append-y (Y #'append-maker)
  "Append function created with Y combinator")
;; Append:1 ends here

;; Map


;; [[file:y-combinator.org::*Map][Map:1]]
(defun map-maker (f)
  "Creates a map function using the given recursion mechanism."
  (lambda (fn lst)
    (if (null lst)
        nil
      (cons (funcall fn (car lst))
            (funcall f fn (cdr lst))))))

(defvar map-y (Y #'map-maker)
  "Map function created with Y combinator")
;; Map:1 ends here

;; Filter


;; [[file:y-combinator.org::*Filter][Filter:1]]
(defun filter-maker (f)
  "Creates a filter function using the given recursion mechanism."
  (lambda (pred lst)
    (cond ((null lst) nil)
          ((funcall pred (car lst))
           (cons (car lst) (funcall f pred (cdr lst))))
          (t (funcall f pred (cdr lst))))))

(defvar filter-y (Y #'filter-maker)
  "Filter function created with Y combinator")

;; CONTINUE meta-joke: List operations on word characters
(let* ((word "CONTINUE")
       (char-list (string-to-list word))
       (vowels '(?C ?O ?N ?T ?I ?N ?U ?E))
       (consonants (funcall filter-y (lambda (c) (not (memq c vowels))) char-list))
       (doubled (funcall map-y (lambda (c) (* c 2)) char-list)))
  (message "CONTINUE: original length=%d, filtered consonants=%d, doubled sum=%d"
           (funcall length-y char-list) 
           (funcall length-y consonants)
           (apply '+ doubled)))
;; Filter:1 ends here
