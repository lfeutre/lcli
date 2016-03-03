(defmodule lcli-exceptions
  (export all))

(include-lib "clj/include/compose.lfe")

(defun make-error (msg)
  `#(error ,msg))

(defun make-error (err msg)
  `#(,err ,msg))

(defun make-error-info (err preamble msg)
  (->> msg
       (list preamble)
       (io_lib:format "~s ~p")
       (make-error err)))

(defun arg-parse (msg)
  (make-error-info 'arg-parse-error "Can't parse args:" msg))

(defun opt-parse (msg)
  (make-error-info 'opt-parse-error "Can't parse options:" msg))
