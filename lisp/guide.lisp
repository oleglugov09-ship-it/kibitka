;;;; The guide to the game: a booklet in the documents folder (G while the folder is open; ← → to leaf through).
;;;; The pages are plain text; the tables of goods and booths come from the same lists the game runs on,
;;;; so the booklet does not lie when the numbers and towns change.

(in-package #:kiosk-trip)

(defvar *guide-open* nil)
(defvar *guide-page* 0 "The index of the current spread.")
(defvar *guide-parts* nil)

(defun guide-trade-page ()
  "Return the page text on what is made where (cheap there) and what is short where (expensive there)."
  (format nil "~{~a~%~}"
          (loop for i from 0 below (length *town-trade*)
                collect (format nil "~a: делают ~{~a~^, ~}; ищут ~{~a~^, ~}." (town-name i)
                                (mapcar #'good-name (town-makes i)) (mapcar #'good-name (town-wants i))))))

(defvar *gimmick-notes* '()
  "What each booth is good for, as (index, line for the guide) entries.")

(defun guide-gimmick-page ()
  (format nil "~{~a~%~}"
          (append (loop for (i nil nil nil town) in *gimmick-plan*
                        collect (format nil "~a — ~a." (town-name (or town i)) (second (assoc i *gimmick-notes*))))
                  *guide-booth-extras*)))

(defvar *guide-pages* '()
  "The pages of the guide book, in order: entries of (title body).
The body is the page text, or a keyword for a page the game writes itself from the content tables:
:trade (what is made and wanted where), :booths, :gnomes, :gnomes-2.")

(defvar *guide-booth-extras* '()
  "Extra lines for the :booths page of the guide: places that are special but are not booths.")

(defun guide-pages ()
  "Return the pages of the booklet as (title . text) pairs.
A spread shows two of them."
  (loop for (title body) in *guide-pages*
        collect (cons title (case body
                              (:trade (guide-trade-page)) (:booths (guide-gimmick-page))
                              (:gnomes (guide-gnome-page)) (:gnomes-2 (guide-gnome-page-2))
                              (t body)))))

(defun make-guide (bg)
  "Build the booklet over the desk with papers, on BG: a spread of two pages."
  (let ((box (gdext:make-object "Control")) (ink '(.2 .16 .1)))
    (node-add-child bg box)
    (doc-card box "guide" 20 10 600 340)
    (flet ((page (x) (let ((title (hud-label box "" x 26 9 '(.42 .12 .1))) (body (hud-label box "" x 46 7 ink)))
                       (object-set body "autowrap_mode" 3)
                       (object-set body "size" (vector2 264 280))
                       (list title body))))
      (setf *guide-parts* (list :box box :left (page 40) :right (page 336)
                                :foot (hud-label box "" 210 330 7 '(.42 .36 .28)))))
    (object-set box "visible" nil)))

(defun show-guide ()
  (let* ((pages (guide-pages)) (spreads (ceiling (length pages) 2)))
    (setf *guide-page* (max 0 (min (1- spreads) *guide-page*)))
    (loop for key in '(:left :right) for k from 0
          do (let ((p (nth (+ (* 2 *guide-page*) k) pages)))
               (destructuring-bind (title body) (getf *guide-parts* key)
                 (object-set title "text" (if p (car p) ""))
                 (object-set body "text" (if p (cdr p) "")))))
    (object-set (getf *guide-parts* :foot) "text" (format nil "~d из ~d · A и D или стрелки — листать · G — закрыть" (1+ *guide-page*) spreads))
    (object-set (getf *guide-parts* :box) "visible" *guide-open*)))

(defun guide-key (code)
  "Handle the key CODE while the folder is open.
Return T when the booklet consumed the key."
  (cond ((= code 71) (setf *guide-open* (not *guide-open*)) (show-guide) t)                     ; G
        ((not *guide-open*) nil)
        ((member code '(4194319 65)) (decf *guide-page*) (show-guide) t)                        ; ← A
        ((member code '(4194321 68 32)) (incf *guide-page*) (show-guide) t)                     ; → D Space
        (t (setf *guide-open* nil) (show-guide) nil)))                                          ; anything else closes both the booklet and the folder
