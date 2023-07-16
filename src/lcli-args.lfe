(defmodule lcli-args
  (export all))

(defun raw ()
  "Using the Erlang function ``#'init:get_plain_arguments/0``, extract the
  arguments passed to a script via the command line as well as the name of
  the script itself. These are returned in a property list of the form:

  ```cl
  (#(script \"...\")
   #(args (...)))
  ```"
  (raw (init:get_plain_arguments)))

(defun raw
  (('())
    (raw '("")))
  (((cons script args))
    `(#(script ,script)
      #(args ,args))))

(defun script ()
  "Using the Erlang function ``#'init:get_plain_arguments/0``, extract the
  the name of the script that was passed arguments via the command line."
  (script (raw)))

(defun script
  ((`(#(script ,script) ,_))
    script)
  ((result)
    (lcli-error:arg-parse result)))

(defun get ()
  "Using the Erlang function ``#'init:get_plain_arguments/0``, extract the
  arguments passed to a script via the command line."
  (get (raw)))

(defun get
  ((`(,_ #(args ,args)))
      args)
  ((result)
      (lcli-error:arg-parse result)))
