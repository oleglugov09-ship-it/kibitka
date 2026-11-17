;;;; Random numbers. The world must come out the same for every player, but ECL's built-in generator
;;;; differs between desktop and browser (64 and 32 bits): one seed gives different towns. So wherever
;;;; the shared world depends on randomness, our own generator does the work (Park-Miller, integers
;;;; only): binding *SEED* turns it on, usually as (let ((*seed* (rs-for place))) ...). Outside such a
;;;; binding RANDOM is the implementation's ordinary generator: for cosmetics that differ per player.

(in-package #:kiosk-trip)

(defvar *seed* nil "The state of our own generator while it is in use, a number; NIL means the implementation's generator.")

(defun next-seed () (setf *seed* (mod (* *seed* 48271) 2147483647)))

(defun random (limit &optional (state cl:*random-state*))
  "Return a random number below LIMIT like CL:RANDOM, optionally from STATE.
While *SEED* is bound, use our own generator, which is the same on every platform."
  (cond ((null *seed*) (cl:random limit state))
        ((integerp limit) (mod (next-seed) limit))
        (t (float (* limit (/ (float (next-seed) 1d0) 2147483647d0)) limit))))

(defun rs-for (n)
  "Return the seed for place N.
It is the same for all players that share one world seed."
  (let ((*seed* (1+ (mod (+ (* 7919 (or *trip-seed* 1)) (* 31 (round n))) 2147483645))))
    (dotimes (i 3) (next-seed))            ; the first numbers of nearby seeds look alike - skip them
    *seed*))
