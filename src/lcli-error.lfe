(defmodule lcli-error
  (export all))

(defun make (msg)
  `#(error ,msg))

(defun make (err msg)
  `#(,err ,msg))

(defun msg (err preamble msg)
  (clj:->> msg
          (list preamble)
          (io_lib:format "~s ~p")
          (make err)))

(defun arg-parse (msg)
  (msg 'arg-parse-error "Can't parse args:" msg))

(defun opt-parse (msg)
  (msg 'opt-parse-error "Can't parse options:" msg))
