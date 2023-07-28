(defmodule lcli
  (export
   (args 0) (args 1)
   (parse 1) (parse 2)
   (start 0)
   (usage 1)
   (version 0)
   (versions 0))
  ;; Just to make xref shut up about an include
  (export
   (--loaded-lcli-records-- 0)))

(include-lib "logjam/include/logjam.hrl")
(include-lib "lcli/include/records.lfe")

;;; API functions

(defun args ()
  "Using the Erlang function `#'init:get_plain_arguments/0`, extract the
  arguments passed to a script via the command line as well as the name of
  the script itself. These are returned as an lcli record."
  (args (init:get_plain_arguments)))

(defun args
  (('())
   (args '("")))
  (((cons script args))
   (make-plain-args script script
                    args args)))

(defun parse (data)
  (parse data (args)))

(defun parse
  ;; Non-list, map input
  ((data args) (when (is_map data))
   (parse (lcli-type:map->record data) args))
  ;; App
  (((= (match-app) data) args)
   (parse-app data args))
  ;; Command
  (((= (match-command) data) args)
   (parse-command data args))
  ;; Everything else
  ((data args)
   (cond ((lcli-type:maplist? data) (parse (lcli-type:maps->records data) args))
         ((lcli-type:recordlist? data) (parse-recordlist data args))
         ('true (lfe_io:format "Could not parse input: ~p~n" (list args))))))

(defun start ()
  (logjam:set-dev-config))

(defun usage
  ;; Non-list, map input
  ((input) (when (is_map input))
   (usage (lcli-type:map->record input)))
  ;; App
  (((match-app name n title t desc d options os args as commands cs))
   (app-usage n t d os as cs))
  ;; Command
  (((match-command name n title t desc d options os args as))
   (cmd-usage n t d os as))
  ;; Everything else
  ((input)
   (cond ((lcli-type:maplist? input) (usage (lcli-type:maps->records input)))
         ((lcli-type:recordlist? input) (basic-usage input))
         ('true (lfe_io:format "Could not match input for usage type: ~p~n" (list input))))))

;;; Metadata

(defun version ()
  (lcli-vsn:get))

(defun versions ()
  (lcli-vsn:all))

;;; Private functions

(defun parse-app (data args)
  (io:format "App-parsing TBD~n"))

(defun parse-command (data args)
  (io:format "Command-parsing TBD~n"))

(defun parse-recordlist (data args)
  (let ((result (lcli-getopt:parse data args)))
    (io:format "Record-list-parsing TBD~n~p~n" (list result))
    result))

(defun app-usage (name title desc opts args cmds)
  (log-debug "Got opts: ~p" (list opts))
  (let* ((desc (lcli-usage:description desc))
         (synop (lcli-usage:synopsis name opts args))
         (opts (lcli-usage:options opts args))
         (title (io_lib:format "~s - ~s" (list name title)))
         (cmds (lcli-usage:commands cmds))
         (help (make-help title title
                          desc desc
                          synopsis synop
                          options opts
                          commands cmds)))
    (lfe_io:format "~s~n" (list (lcli-usage:compile help 'app-manpage)))))

(defun cmd-usage (name title desc opts args)
  (let* ((desc (lcli-usage:description desc))
         (synop (lcli-usage:synopsis name opts args))
         (opts (lcli-usage:options opts args))
         (title (io_lib:format "~s - ~s" (list name title)))
         (help (make-help title title
                          desc desc
                          synopsis synop
                          options opts)))
    (lfe_io:format "~s~n" (list (lcli-usage:compile help)))))

(defun basic-usage (opts)
  (let (((match-plain-args script name args args) (args)))
    (basic-usage name opts args)))

;; commented out for Xref, for now
;;(defun basic-usage (name opts)
;;  (let (((match-plain-args args args) (args)))
;;    (basic-usage name opts args)))

(defun basic-usage (name opts args)
  (let* (((match-plain-args args args) (args))
         (synop (lcli-usage:synopsis name opts args))
         (opts (lcli-usage:options opts args))
         (help (make-help title name
                          synopsis synop
                          options opts)))
    (lfe_io:format "~s~n" (list (lcli-usage:compile help)))))
