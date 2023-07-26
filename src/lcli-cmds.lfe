(defmodule lcli-cmds
  (export
   (commands? 1)
   (filter 1)
   (get 1)
   (usage 1))
  ;; Just to make xref shut up about an include
  (export
   (--loaded-lcli-records-- 0)))

(include-lib "lcli/include/records.lfe")

(defun key () 'commands)

(defun commands?
  "Check to see if the given CLI setup data has a `commands` element
  defined."
  (('())
   'false)
  ((data)
   (cond ((lcli-util:speclist? data) (speclist-has-commands? data))
         ((lcli-util:maplist? data) (maplist-has-commands? data))
         ((is_record data 'app) (app-has-commands? data))
         ((is_tuple data) (spec-has-commands? data))
         ((is_map data) (map-has-commands? data))
         ('true 'false))))

(defun get (data)
  (cond ((lcli-util:speclist? data) (speclist-commands data))
        ((lcli-util:maplist? data) (maplist-commands data))
        ((is_record data 'app) (app-commands data))
        ((is_tuple data) (spec-commands data))
        ((is_map data) (map-commands data))
        ('true '())))

(defun filter (specs)
  "In a list of specs, return only those that are true option specs, not those
  that are lcli-specific 'commands specs'."
  (lists:filtermap #'spec-without-commands?/1
                   (lcli-spec:-> specs)))

(defun usage
  ;; Check for app
  (((match-app name n title t description d options os args as commands cs))
   (usage n t d os as cs))
  ((`#m(name ,n title ,t description ,d options ,os args ,as commands ,cs))
   (usage n t d os as cs))
  ;; Check for command
  (((match-command name n title t description d options os args as))
   (usage n t d os as))
  ((`#m(name ,n title ,t description ,d options ,os args ,as))
   (usage n t d os as))
  ((data)
   (lfe_io:format "Could not match input for usage type: ~p~n" (list data))))

;;; Private functions

(defun usage (name title desc opts args)
  (let* ((desc (lcli-usage:description desc))
         (synop (lcli-usage:synopsis name opts args))
         (opts (lcli-usage:options opts args))
         (title (io_lib:format "~s - ~s" (list name title)))
         (help (make-help title title
                          description desc
                          synopsis synop
                          options opts)))
    (io:format "~s~n" (list (lcli-usage:compile help)))))

(defun usage (name title desc opts args cmds)
  (let* ((desc (lcli-usage:description desc))
         (synop (lcli-usage:synopsis name opts args))
         (opts (lcli-usage:options opts args))
         (title (io_lib:format "~s - ~s" (list name title)))
         (cmds (lcli-usage:commands cmds))
         (help (make-help title title
                          description desc
                          synopsis synop
                          options opts
                          commands cmds)))
    (io:format "~s~n" (list (lcli-usage:compile help 'app-manpage)))))

(defun app-has-commands?
  (((match-app commands '()))
   'false)
  (((match-app commands _))
   'true))

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

(defun speclist-commands
  "This function is intended to be used with specs that are in the form defined
  by the Erlang getopt library."
  ((`(,spec . ,rest))
   (let ((cmds (spec-commands spec)))
     (if (> (length cmds) 0)
       cmds
       (speclist-commands rest)))))

(defun spec-commands
  ((spec) (when (is_tuple spec))
   (if (andalso
        (== (element 1 spec) (key))
        (is_list (element 2 spec)))
     (element 2 spec)
     '())))

(defun maplist-has-commands?
  (('())
   'false)
  ((`(,spec . ,rest))
   (if (map-has-commands? spec)
     'true
     (maplist-has-commands? rest))))

(defun map-has-commands?
  ((spec) (when (is_map spec))
   (andalso
    (maps:is_key (key) spec)
    (is_list (mref spec (key))))))

(defun maplist-commands
  ((`(,spec . ,rest))
   (let ((cmds (map-commands spec)))
     (if (> (length cmds) 0)
       cmds
       (maplist-commands rest)))))

(defun map-commands
  ((spec) (when (is_map spec))
   (if (andalso
        (maps:is_key (key) spec)
        (is_list (mref spec (key))))
     (mref spec (key))
     '())))

(defun spec-without-commands?
  "A predicate of a form suitable for use in `lists:filtermap/2`: if the
  provided spec is not a 'commands' return `#(true spec)` otherwise return
  `false`."
  ((spec) (when (is_map spec))
   (not (map-has-commands? spec)))
  ((spec) (when (is_tuple spec))
   (not (spec-has-commands? spec))))
