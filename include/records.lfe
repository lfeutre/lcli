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
  (subcommands '() (list))
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
(defrecord parsed
  (app-name "" (string))
  (script "" (string))
  (app "" (string))
  (commands '() (list))
  (options #m() (map)) ; note that parsed options are different than option definitions
  (args '() (list))
  (error '() (string)))

(defun --loaded-lcli-records-- ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).

  This function needs to be the last one in this include."
  'ok)
