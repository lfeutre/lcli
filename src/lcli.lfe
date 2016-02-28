(defmodule lcli
  (export all))

(defun get-args ()
  (let ((args (init:get_plain_arguments)))
    `(#(script ,(car args))
      #(args ,(lists:nthtail 1 args)))))
