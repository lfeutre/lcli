(defmodule lcli-exceptions
  (export all))

(defun make-error (msg)
  `#(error ,msg))

(defun make-error (err msg)
  `#(,err ,msg))

(defun arg-parse (msg)
  (make-error 'arg-parse-error
              (++ "Can't parse args: " msg)))

(defun opt-parse (msg)
  (make-error 'opt-parse-error
              (++ "Can't parse options: " msg)))
