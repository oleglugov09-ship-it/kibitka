;;;; A probe for the collector of the browser build, for compiled code (the one in dev.lisp holds its data in
;;;; variables and forces the collection; this one builds values inside the arguments of a call and lets the
;;;; collector come when it will). If a value that is only an argument waiting for its call can be freed, some of
;;;; what is built here comes back damaged - or the page dies with "not a lisp data object".
;;;;
;;;; From a ?dev page: tools/web-live.py lisp '(kiosk-trip::gc-probe 200000)'  =>  the number of damaged entries.

(in-package #:kiosk-trip)

(defun gc-probe (&optional (n 200000))
  "Build N small structures, each inside the arguments of one call, and return how many came back damaged."
  (let ((bad 0))
    (dotimes (i n bad)
      (let ((x (list (cons i (make-string 16 :initial-element #\a))
                     (cons (1+ i) (make-list 6 :initial-element i))
                     (cons (+ 2 i) (make-string 16 :initial-element #\b)))))
        (unless (and (consp (first x)) (eql (car (first x)) i) (stringp (cdr (first x))) (char= (char (cdr (first x)) 0) #\a)
                     (consp (second x)) (eql (car (second x)) (1+ i)) (consp (cdr (second x))) (eql (cadr (second x)) i)
                     (consp (third x)) (eql (car (third x)) (+ 2 i)) (stringp (cdr (third x))) (char= (char (cdr (third x)) 15) #\b))
          (incf bad))))))
