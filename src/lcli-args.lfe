(defmodule lcli-args
  (export all))

(defun get-raw ()
  "Using the Erlang function ``#'init:get_plain_arguments/0``, extract the
  arguments passed to a script via the command line as well as the name of
  the script itself. These are returned in a property list of the form:

  ```cl
  (#(script \"...\")
   #(args (...)))
  ```"
  (get-raw (init:get_plain_arguments)))

(defun get-raw
  (('())
    (get-raw '("")))
  (((cons script args))
    `(#(script ,script)
      #(args ,args))))

(defun get-script ()
  "Using the Erlang function ``#'init:get_plain_arguments/0``, extract the
  the name of the script that was passed arguments via the command line."
  (get-script (get-raw)))

(defun get-script
  ((`(#(script ,script) ,_))
    script)
  ((result)
    (lcli-error:arg-parse result)))

(defun get ()
  "Using the Erlang function ``#'init:get_plain_arguments/0``, extract the
  arguments passed to a script via the command line."
  (get (get-raw)))

(defun get
  ((`(,_ #(args ,args)))
      args)
  ((result)
      (lcli-error:arg-parse result)))
