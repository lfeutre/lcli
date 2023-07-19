(defmodule lcli-cmds
  (export all))

(defun key () 'commands)

(defun commands?
  "Check to see if the given list of options specs has a `commands` element
  defined."
  (('())
   'false)
  ((specs)
   (if (lcli-util:speclist? specs)
     (speclist-has-commands? specs)
     (maps-has-commands? specs))))

(defun speclist-has-commands?
  "This function is intended to be used with specs that are in the form defined
  by the Erlang getopt library."
  (('())
   'false)
  ((`(,spec . ,rest))
   (if (spec-has-commands? spec)
     'true
     (speclist-has-commands? rest))))

(defun spec-has-commands?
  ((spec) (when (is_tuple spec))
   (andalso
    (== (element 1 spec) (key))
    (is_list (element 2 spec)))))

(defun maps-has-commands?
  (('())
   'false)
  ((`(,spec . ,rest))
   (if (map-has-commands? spec)
     'true
     (maps-has-commands? rest))))

(defun map-has-commands?
  ((spec) (when (is_map spec))
   (andalso
    (maps:is_key (key) spec)
    (is_list (mref spec (key))))))

(defun spec-without-commands?
  "A predicate of a form suitable for use in `lists:filtermap/2`: if the
  provided spec is not a 'commands' return `#(true spec)` otherwise return
  `false`."
  ((spec) (when (is_map spec))
   (not (map-has-commands? spec)))
  ((spec) (when (is_tuple spec))
   (not (spec-has-commands? spec))))

(defun filter (specs)
  "In a list of specs, return only those that are true option specs, not those
  that are lcli-specific 'commands specs'."
  (lists:filtermap #'spec-without-commands?/1
                   (lcli-spec:->maps specs)))
