;;;; Cross-compile gdecl and the game (the ASDF system $SYSTEM) for wasm32,
;;;; with the host ECL and ECL's target info for the web build of ECL. Writes
;;;; build-web/application.a, with init function init_lib_APPLICATION as for
;;;; the desktop build. Run by web/build.sh.
;;;;
;;;; Two passes, both with the overrides of cross-overrides.lisp (wasm32's FFI
;;;; sizes and builtin sizes): A loads the system into the host with ASDF (its
;;;; own fasl cache, see build.sh), for the macros and constants; B compiles
;;;; each file for the target, in ASDF's order.

(require :asdf)
(require :cmp)

(defparameter *root* (pathname (ext:getenv "PROJECT_ROOT")))
(defparameter *system* (ext:getenv "SYSTEM"))
(defparameter *gdecl* (pathname (ext:getenv "GDECL")))
(defparameter *target-info* (merge-pathnames "target-info.lsp" (pathname (ext:getenv "ECL_TARGET"))))
(defparameter *build* (merge-pathnames (or (ext:getenv "BUILD_DIR") "build-web/") *root*))

(load (merge-pathnames "web/cross-overrides.lisp" *root*))
(push *gdecl* asdf:*central-registry*)
(push (merge-pathnames "lisp/" *root*) asdf:*central-registry*)
(ensure-directories-exist *build*)

(defun source-files (system)
  "The Lisp source files of SYSTEM and the systems it depends on, in load order."
  (loop for component in (asdf:required-components (asdf:find-system system)
                                                   :other-systems t
                                                   :component-type 'asdf:cl-source-file
                                                   :goal-operation 'asdf:load-op)
        when (typep component 'asdf:cl-source-file)
          collect (asdf:component-pathname component)))

(defun cross-compile (source)
  (let* ((dir (car (last (pathname-directory source))))
         (output (merge-pathnames (make-pathname :name (format nil "~a--~a" dir (pathname-name source))
                                                 :type "o")
                                  *build*)))
    (format t "~&;;; cross: ~a~%" (enough-namestring source *root*))
    (install-cross-overrides)
    (let ((compiled (compile-file source :output-file output :system-p t :verbose nil :print nil)))
      (unless compiled
        (error "Cross-compiling ~a failed" source))
      (install-cross-overrides)
      compiled)))

;; Pass A: gdecl's own files define API-BUILTIN-SIZE, so the overrides go back
;; after each file is loaded
(defmethod asdf:perform :after ((operation asdf:load-op) (file asdf:cl-source-file))
  (install-cross-overrides))
(install-cross-overrides)
(handler-bind ((warning #'muffle-warning))
  (asdf:load-system *system*))

(handler-bind ((warning #'muffle-warning))
  (let ((saved (si::trap-fpe 'last nil)))
    (si::trap-fpe t nil)
    (unwind-protect
         (with-compilation-unit (:target *target-info*)
           (let ((objects (mapcar #'cross-compile (source-files *system*))))
             (c:build-static-library (merge-pathnames "application.a" *build*)
                                     :lisp-files objects
                                     :init-name "init_lib_APPLICATION")
             (format t "~&;;; built ~a from ~d files~%"
                     (merge-pathnames "application.a" *build*) (length objects))))
      (si::trap-fpe saved t))))
(si:quit 0)
