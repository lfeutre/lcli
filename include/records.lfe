(defrecord app
  (name "app" (string))
  (title "" (string))
  (desc "" (string))
  (options '() (list))
  (args '() (list))
  (commands '() (list))
  (lib-opts '#m(with-color true) (map)))

(defrecord group
  (name 'undefined (atom))
  (summary "" (string))
  (order 0 (integer)))

(defrecord command
  (name "" (string))
  (title "" (string))
  (desc "" (string))
  (commands '() (list))
  (options '() (list))
  (args '() (list))
  (order 0 (integer))
  (groups '() (list))
  ;;(runner 'undefined (lambda)))
  (runner 'undefined (any)))

(defrecord option
  (name 'undefined (atom))
  (short 'undefined (integer))
  (long 'undefined (string))
  (help 'undefined (string))
  (val-type 'undefined (atom))
  (default 'undefined (any))
  (required? 'false (atom))
  (multi-valued? 'false (atom))
  (delim "," (string))
  ;;(value-parser 'undefined (lambda))
  (value-parser 'undefined (any))
  (env "" (string))
  (order 0 (integer))
  (groups '() (list)))

(defrecord arg
  (name "" (string))
  (required? 'false (atom))
  (help "" (string)))

(defrecord help
  (title-heading "NAME" (string))
  (title "" (string))
  (synopsis-heading "SYNOPSIS" (string))
  (synopsis "" (string))
  (desc-heading "DESCRIPTION" (string))
  (desc "" (string))
  (options-heading "OPTIONS" (string))
  (options "" (string))
  (commands-heading "COMMANDS" (string))
  (commands "" (string))
  (additional "" (string)))

(defrecord plain-args
  (script "" (string))
  (args '() (list)))

;; The options and args fields from this record are initially populated when
;; obtaining results from the third-party Erlang getopt library. Funtions that
;; make this call update the other fields once the results from getopt have
;; been parsed.
;;
;; An app's options are parsed separately from a commands options, with the
;; parsed results being stored in the 'app' field.
;;
;; Commands may have an arbitrarily deep number of levels (subcommands). For
;; convenience, recusive parsing results are stored in a flat list of tuples:
;;
;; 1. the first element of each tuple is the command/subcommand
;; 2. the second element is the map of parsed options associated with that
;;    command
;; 3. the third element is the remaining args that have no yet been parsed
;;
;; The last (sub)command parsed will either have a third element whose list
;; is empty, or a list containing the remaining args that are not associated
;; with a legal subcommand in the chain of commands.
;;
;; Finally, his ordered list of (atom, map, list) tuples is stored in the
;; 'commands' field.
;;
;; For the most part, a simple script with no commands (namely, a list of
;; options), is essentially an app; as such, its parsed options  are stored in
;; the 'options' field just like the app's options.
;;
;; Lastly, please note that parsed 'options' are different from the list of
;; 'option' definitions used when defining a CLI's set of allowed options.
(defrecord parsed
  (app-name "" (string))
  (script "" (string))
  (app #m() (map))
  (commands '() (list))
  (options #m() (map))
  (args '() (list))
  (error '() (string)))

(defun --loaded-lcli-records-- ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).

  This function needs to be the last one in this include."
  'ok)
