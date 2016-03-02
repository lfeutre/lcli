(defmodule lcli-opts
  (export all))

(defun parse-opts (spec raw-args)
  "Parse only the options in the spec, not the commands."
  (case (getopt:parse spec raw-args)
    (`#(ok ,result)
      result)
    (err
      (lcli-exceptions:opt-parse err))))

(defun get-opt (key opts)
  "Extract the value of an option associated with the given key.

  Note that the indended use for this function is for extracting values from
  options that have already been parsed using the provided specs and arguments
  passed via the command line. This this function is essentially a permissive
  version of ``proplists:get_value`` it may also be used for other data."
  (case (lists:keyfind key 1 opts)
    (`#(,_ ,value)
      value)
    ('false
      'undefined)))

(defun boolean-opt? (key opts)
  "Test for the presence of a boolean option."
  (andalso (is_list opts)
           (lists:member key opts)))

(defun help? (opts)
  "Test for the presence of the 'help' option (boolean)."
  (boolean-opt? 'help opts))

(defun filter-specs (specs)
  "In a list of specs, return only those that are true option specs, not those
  that are lcli-specific 'commands specs'."
  (lists:filtermap #'lcli-cmds:not-commmands?/1 specs))
