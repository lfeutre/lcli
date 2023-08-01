(defmodule lcli-parse
  (export
   (app 2) (app2 2)
   (find-command 2) (find-command 3)
   (commands 2)
   (recordlist 2))
  ;; Utility functions
  (export
   (get-command-def 2)
   (getopt 2)
   (legal-commands 1))
  ;; Just to make xref shut up about an include
  (export
   (--loaded-lcli-records-- 0)))

(include-lib "logjam/include/logjam.hrl")
(include-lib "lcli/include/records.lfe")

(defun app
  (((match-app name n options os commands cs) args)
   ;; XXX There's lots more to do here with commands, etc.
   (let ((result (lcli-getopt:parse os args)))
     ;;     (update-parsed result app-name n)
     result
     )))

;; XXX Temp function; once it's sorted, it will be backwards-compatible
;; with app, and the two can be merged.
(defun app2
  (((match-app name n options os commands cs) args)
   ;; XXX There's lots more to do here with commands, etc.
   (let* ((parsed-app (lcli-getopt:parse os args))
          (cmds (commands cs (parsed-args parsed-app))))
     (update-parsed parsed-app
                    app-name n
                    commands cmds
                    ;;args (parsed-args cmds)
                    ))))

(defun commands (cmd-defs parent-args)
  (commands cmd-defs
            parent-args
            '()))
  
(defun commands
  (('() _ acc)
   acc)
  ((_ '() acc)
   acc)
  ((cmd-defs parent-args acc)
   (let* ((legal-cmds (legal-commands cmd-defs))
          (`#(,_pre-cmd-args ,name ,post-cmd-args) (find-command legal-cmds parent-args))
          (cmd-def (get-command-def cmd-defs name))
          (parsed (getopt cmd-def parent-args)))
     (commands (command-commands cmd-def)
               post-cmd-args
               (++ acc `(#m(name ,name
                            parsed ,parsed
                            args ,post-cmd-args)))))))

(defun find-command (legal-cmds args)
  "This function takes a list of supported commands (sibling to each other; not
  commands from different levels) and looks for the first element of the args
  list that matches one of the commands. When it finds a match, the search ends
  with a three-tuple being returned with the following structure:

  1. first element: the arguments that precede the command (a list of strings)
  2. second element: the command itself (a string)
  3. third element: the argument remaining at the point when a legal command
     was found in the args list

  If there is no command in the args, the first element in the reults tuple will
  be comprised of the entire list of args.

  If the first element in the args list is a legal command, the first element
  in the results tuple will be empty and the last element of the results will
  contain the args after the command. The last element of the results will be
  empty if the passed args consist only of a single legal command."
  (find-command legal-cmds args '()))

(defun find-command
  ((_ '() acc)
   `#(,acc "" '()))
  ((legal-cmds `(,head . ,tail) acc)
   (if (lists:member head legal-cmds)
     `#(,acc ,head ,tail)
     (find-command legal-cmds tail (++ acc (list head))))))

(defun recordlist
  "Note that record lists are only useful for simple scripts with no commands.
  Command handling is not performed here, and the functionality of parsing
  a record list is essentially a very thin layer over the simple opt parsing
  functionality provided by the Erlang getopt library."
  ((data (= (match-plain-args script name) args))
   (let ((result (lcli-getopt:parse data args)))
     ;;(update-parsed-app-name result name)
     result)))

;;; Utility functions

(defun get-command-def
  (('() _)
   '())
  ((`(,head . ,tail) name)
   (if (== (command-name head) name)
     head
     (get-command-def tail name))))

(defun getopt
  (((match-command name n options '()) parent-args)
   (make-parsed args parent-args))
  (((match-command name n options os) parent-args)
   (let ((args (make-plain-args script n args parent-args)))
     (lcli-getopt:parse os args))))

(defun legal-commands (cmd-defs)
  (lists:map (lambda (x) (command-name x)) cmd-defs))

;;; Private functions

