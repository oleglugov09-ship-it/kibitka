;;;; Loaded into the host ECL (64-bit Linux) while the game is cross-compiled,
;;;; by build-web.lisp: what gdecl computes when the code is compiled must be
;;;; the target's, not the host's. The target is $CROSS_TARGET: wasm32 (the web
;;;; build, the default) or win64 (Windows: LLP64, long is 4 bytes).
;;;;
;;;; 1. FFI sizes. gdecl's macros look up FFI:SIZE-OF-FOREIGN-TYPE and
;;;;    FFI::SLOT-POSITION when the code is compiled (struct offsets, element
;;;;    strides, memset sizes). In the host those come from the host's C
;;;;    types: pointers and longs of 8 bytes. These copies of ECL's functions
;;;;    (src/lsp/ffi.lsp) use wasm32's sizes and alignments instead.
;;;; 2. Builtin sizes. GDEXT::API-BUILTIN-SIZE reads build configuration
;;;;    float_64 from extension_api.json; wasm32 is float_32 (String,
;;;;    StringName, NodePath, Object and the packed arrays are smaller).
;;;;
;;;; INSTALL-CROSS-OVERRIDES is called again after each file is compiled and
;;;; loaded, as gdecl's own files define API-BUILTIN-SIZE.

(in-package :cl-user)

(defparameter *win64-elt-types*
  ;; type size alignment, C on Windows x64 (mingw): LLP64
  '((:char 1 1) (:unsigned-char 1 1) (:byte 1 1) (:unsigned-byte 1 1)
    (:int8-t 1 1) (:uint8-t 1 1)
    (:short 2 2) (:unsigned-short 2 2) (:int16-t 2 2) (:uint16-t 2 2)
    (:int 4 4) (:unsigned-int 4 4) (:int32-t 4 4) (:uint32-t 4 4)
    (:long 4 4) (:unsigned-long 4 4)
    (:long-long 8 8) (:unsigned-long-long 8 8) (:int64-t 8 8) (:uint64-t 8 8)
    (:float 4 4) (:double 8 8) (:long-double 16 16)
    (:pointer-void 8 8) (:object 8 8) (:cstring 8 8) (:void 0 1)))

(defparameter *cross-target* (or (ext:getenv "CROSS_TARGET") "wasm32"))

(defparameter *wasm32-elt-types*
  ;; type size alignment, C on wasm32: ILP32, 64-bit types aligned to 8
  '((:char 1 1) (:unsigned-char 1 1) (:byte 1 1) (:unsigned-byte 1 1)
    (:int8-t 1 1) (:uint8-t 1 1)
    (:short 2 2) (:unsigned-short 2 2) (:int16-t 2 2) (:uint16-t 2 2)
    (:int 4 4) (:unsigned-int 4 4) (:int32-t 4 4) (:uint32-t 4 4)
    (:long 4 4) (:unsigned-long 4 4)
    (:long-long 8 8) (:unsigned-long-long 8 8) (:int64-t 8 8) (:uint64-t 8 8)
    (:float 4 4) (:double 8 8) (:long-double 16 16)
    (:pointer-void 4 4) (:object 4 4) (:cstring 4 4) (:void 0 1)))

(defun wasm32-elt (type)
  (or (assoc type (if (string= *cross-target* "win64") *win64-elt-types* *wasm32-elt-types*))
      (error "No wasm32 size for the FFI element type ~s" type)))

(defun wasm32-size-of-foreign-type (name)
  (let* ((size 0) align
         (type (ffi::%convert-to-ffi-type name)))
    (unless type
      (error "Incomplete or unknown foreign type ~A" name))
    (flet ((align-up (n a) (* (ceiling n a) a)))
      (cond ((symbolp type)
             (destructuring-bind (s a) (rest (wasm32-elt type))
               (setf size s align a)))
            ((atom type)
             (error "~A is not a valid foreign type identifier" name))
            ((eq (first type) :struct)
             (setf size (wasm32-slot-position type nil))
             (when (rest type)
               (setf align (reduce #'max (rest type)
                                   :key (lambda (field)
                                          (nth-value 1 (wasm32-size-of-foreign-type (second field))))))
               (setf size (align-up size align))))
            ((eq (first type) :array)
             (multiple-value-bind (elt-size elt-align)
                 (wasm32-size-of-foreign-type (second type))
               (setf size (* (third type) elt-size) align elt-align)))
            ((eq (first type) :union)
             (dolist (field (rest type))
               (multiple-value-bind (s a) (wasm32-size-of-foreign-type (second field))
                 (setf size (max size s) align (max (or align 0) a)))))
            ((eq (first type) '*)
             (destructuring-bind (s a) (rest (wasm32-elt :pointer-void))
               (setf size s align a)))
            ((eq (first type) 'quote)
             (return-from wasm32-size-of-foreign-type
               (wasm32-size-of-foreign-type (second type))))
            (t (error "~A does not denote a foreign type" name))))
    (values size (or align 0))))

(defun wasm32-slot-position (type field)
  (setf type (ffi::%convert-to-ffi-type type))
  (let ((ndx 0) (is-union (eq (first type) :union)))
    (dolist (slot (rest type))
      (multiple-value-bind (slot-size slot-align)
          (wasm32-size-of-foreign-type (cadr slot))
        (setf ndx (* (ceiling ndx slot-align) slot-align))
        (when (eq (car slot) field)
          (return-from wasm32-slot-position (values ndx (cadr slot) slot-size)))
        (unless is-union
          (incf ndx slot-size))))
    (values ndx nil nil)))

(defmacro float-32-api-builtin-size (class-name)
  (let* ((json (funcall (find-symbol "EXTENSION-API-JSON" "EXTENSION-API-JSON")))
         (json-get (find-symbol "JSON-GET" "GDECL-JSON"))
         (configuration (find "float_32" (funcall json-get json "builtin_class_sizes")
                              :key (lambda (sizes) (funcall json-get sizes "build_configuration"))
                              :test #'string=))
         (size (find class-name (funcall json-get configuration "sizes")
                     :key (lambda (size) (funcall json-get size "name"))
                     :test #'string=)))
    (if size
        (funcall json-get size "size")
        (error "No float_32 size of the builtin class ~a" class-name))))

(defun install-cross-overrides ()
  (when (string= *cross-target* "lp64")
    (return-from install-cross-overrides))
  (let ((si:*ignore-package-locks* t))
    (setf (fdefinition 'ffi:size-of-foreign-type) #'wasm32-size-of-foreign-type
          (fdefinition 'ffi::slot-position) #'wasm32-slot-position)
    ;; Godot's builtin sizes: float_32 only for wasm32; win64 is float_64
    (let ((api-builtin-size (and (string= *cross-target* "wasm32")
                                 (find-package "EXTENSION-API-JSON")
                                  (find-symbol "API-BUILTIN-SIZE" "EXTENSION-API-JSON"))))
      (when api-builtin-size
        (setf (macro-function api-builtin-size)
              (macro-function 'float-32-api-builtin-size))))))
