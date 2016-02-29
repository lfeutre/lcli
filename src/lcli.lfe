(defmodule lcli
  (export all))

(defun get-raw-args ()
  (let ((all-args (init:get_plain_arguments)))
    `(#(script ,(car all-args))
      #(args ,(lists:nthtail 1 all-args)))))

(defun get-script ()
  (case (get-raw-args)
    (`(#(script ,script) ,_)
      script)
    (result
      (lcli-exceptions:arg-parse result))))

(defun get-args ()
  (case (get-raw-args)
    (`(,_ #(args ,args))
      args)
    (result
      (lcli-exceptions:arg-parse result))))

(defun parse-opts (spec raw-args)
  (case (getopt:parse_and_check spec raw-args)
    (`#(ok ,result)
      result)
    (err
      (lcli-exceptions:opt-parse err))))

(defun get-opt (key opts)
  "Extract the value of an option associated with the given key."
  (element 2 (lists:keyfind key 1 opts)))

(defun opt? (key opts)
  "Test for the presence of a boolean option."
  (andalso (is_list opts)
           (lists:member key opts)))

(defun help? (opts)
  "Test for the presence of the 'help' option (boolean)."
  (opt? 'help opts))

(defun parse (spec)
  (let ((`(#(script ,script) #(args ,raw-args)) (get-raw-args)))
    (parse spec script raw-args)))

(defun parse (spec cmd raw-args)
  (let ((`#(,opts ,args) (parse-opts spec raw-args)))
    `(#(cmd ,cmd)
      #(opts ,opts)
      #(args ,args))))

(defun usage (spec)
  "Wrap the ``(getopt:usage)`` function, providing the computed script name."
  (usage spec (get-script)))

(defun usage (spec script)
  "Wrap the ``(getopt:usage)`` function."
  (getopt:usage spec script))
