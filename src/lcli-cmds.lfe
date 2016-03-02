(defmodule lcli-cmds
  (export all))

(defun commands? (spec)
  "Differentiate between an option named 'commands' and a list of provided
  commands."
  (andalso (== (element 1 spec) 'commands)
           (is_list (element 2 spec))))

(defun not-commmands? (spec)
  "A prediate of a form suitable for use in ``lists:filtermap/2``: if the
  provided spec is not a 'commands' "
  (if (not (commands? spec))
    `#(true ,spec)
    'false))

(defun has-commands? (specs)
  "Check to see if the given option spec has commands defined."
  (lists:any #'commands?/1 specs))
