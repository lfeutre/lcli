(defmodule lcli-cmds
  (export all))

(defun has-commands? (specs)
  "Check to see if the given list of options specs has a `commands` element
  defined."
  (lists:any #'commands?/1 specs))

(defun commands?
  "Check to see if the given spec entry (a single element of an options spec) is
  a `commands` entry or not.

  This function differentiates between an option named 'commands' and an actual
  `commands` element which provides a list of commands."
  ((spec) (when (is_list spec))
   (andalso (== (element 1 spec) 'commands)
            (is_list (element 2 spec))))
  ((spec) (when (is_map spec))
   (maps:is_key 'commands spec)))

(defun not-commands? (spec)
  "A predicate of a form suitable for use in `lists:filtermap/2`: if the
  provided spec is not a 'commands' return `#(true spec)` otherwise return
  `false`."
  (if (not (commands? spec))
    `#(true ,spec)
    'false))


