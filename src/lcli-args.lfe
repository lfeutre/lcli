(defmodule lcli-args
  (export all))

(defun get-raw-args ()
  "Using the Erlang function ``#'init:get_plain_arguments/0``, extract the
  arguments passed to a script via the command line as well as the name of
  the script itself. These are returned in a property list of the form:

  ```cl
  (#(script \"...\")
   #(args (...)))
  ```"
  (get-raw-args (init:get_plain_arguments)))

(defun get-raw-args
  (('())
    (get-raw-args '("")))
  (((cons script args))
    `(#(script ,script)
      #(args ,args))))

(defun get-script ()
  "Using the Erlang function ``#'init:get_plain_arguments/0``, extract the
  the name of the script that was passed arguments via the command line."
  (get-script (get-raw-args)))

(defun get-script
  ((`(#(script ,script) ,_))
    script)
  ((result)
    (lcli-exceptions:arg-parse result)))

(defun get-args ()
  "Using the Erlang function ``#'init:get_plain_arguments/0``, extract the
  arguments passed to a script via the command line."
  (get-args (get-raw-args)))

(defun get-args
  ((`(,_ #(args ,args)))
      args)
  ((result)
      (lcli-exceptions:arg-parse result)))
