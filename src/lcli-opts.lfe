(defmodule lcli-opts
  (export
   (args 1)
   (get 2)
   (help? 1)
   (parse 2)
   (usage 2)))

(defun args (parsed)
  (mref parsed 'args))

(defun get (key parsed)
  "Extract the value of an option associated with the given key.

  Note that the indended use for this function is for extracting values from
  options that have already been parsed using the provided specs and arguments
  passed via the command line. This this function is essentially a permissive
  version of `proplists:get_value` it may also be used for other data."
  (maps:get key (mref parsed 'opts) 'undefined))

(defun help? (parsed)
  "Test for the presence of the 'help' option (boolean)."
  (if (== (get 'help parsed) 'undefined)
    'false
    'true))

(defun parse
  "Parse the options in a given spec by calling `getopt:parse`. Returns a
  map of parsed options and trailing args.

  Note that this function will fail if the passed 'specs' contains
  non-option data such as 'commands'."
  ((specs raw-args)
   (case (getopt:parse (lcli-spec:maps-> specs) raw-args)
     (`#(ok #(,opts ,args))
      `#m(opts ,(parsed-opts->map opts)
          args ,args))
     (err
      (lcli-error:opt-parse err)))))

(defun usage (specs script)
  (getopt:usage (lcli-spec:maps-> specs) script))

;;; Private functions

(defun parsed-opts->map (opts)
  "Convert parsed CLI opts to a map."
  (maps:from_list
   (lists:map #'parsed-opt->tuple/1 opts)))

(defun parsed-opt->tuple
  "Ensure parsed opt is a tuple."
  ((opt) (when (is_tuple opt))
   opt)
  ((opt) (when (is_atom opt))
   `#(,opt true)))
