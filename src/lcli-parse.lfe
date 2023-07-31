(defmodule lcli-parse
  (export
   (app 2) (app2 2)
   (command 2) (command 3)
   (commands 2)
   (recordlist 2))
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

(defun commands (cmd-defs app-args)
  "Since any command may have a subcommand, the parsing of commands is a
  bredth-first operation:
  
  * the app's args are examined for the first one that matches a command
    name
  * that command's def's 'options' are parsed
  * if the command has any entries in its own 'commands' field, then the
    remaining args are searched for a match with any of the legal
    subcommands.
  * this process is followed recursively until there are no more command
    options with their own commands (subcommands).

  At which point, the accumulated list of commands (subcommands) with their
  associated parsed options is returned.

  The list of passed command definitions are converted to a map for internal
  use, with their keys being the command names in each of the defs."
  (let ((lookup (maps:from_list
                 (lists:map (lambda (r) (command-lookup r app-args))
                            cmd-defs))))
    ))

(defun command (legal-commands args)
  (command legal-commands args '()))

(defun command
  ((_ '() acc)
   `#(,acc "" '()))
  ((legal-commands `(,head . ,tail) acc)
   (if (lists:member head legal-commands)
     `#(,acc ,head ,tail)
     (command legal-commands tail (++ acc (list head))))))

(defun recordlist
  "Note that record lists are only useful for simple scripts with no commands.
  Command handling is not performed here, and the functionality of parsing
  a record list is essentially a very thin layer over the simple opt parsing
  functionality provided by the Erlang getopt library."
  ((data (= (match-plain-args script name) args))
   (let ((result (lcli-getopt:parse data args)))
     ;;(update-parsed-app-name result name)
     result)))

;;; Private functions

(defun command-lookup (record app-args)
  `#(,(command-name record)
     #m(record ,record
        parsed ,(lcli-getopt:parse (command-options record) app-args))))