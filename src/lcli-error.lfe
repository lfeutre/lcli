(defmodule lcli-error
  (export all))

(defun make (msg)
  (make 'error msg))

(defun make (err msg)
  `#(,err ,(lists:flatten msg)))

(defun format (err preamble msg)
  (clj:->> msg
          (list preamble)
          (io_lib:format "~s ~p")
          (make err)))

(defun format (err msg)
  (clj:->> (list msg)
           (io_lib:format "~s")
           (make err)))

(defun arg-parse (msg)
  (format 'arg-parse-error "Can't parse args:" msg))

(defun opt-parse (msg)
  (format 'opt-parse-error "Can't parse options:" msg))

(defun invalid-spec (msg)
  (format 'invalid-spec msg))
