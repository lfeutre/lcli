(defrecord app
  (name "app" (string))
  (with-colour 'true (atom)))

(defrecord group
  (name "" (string))
  (order 0 (integer)))

(defrecord command
  (name "" (string))
  (title "" (string))
  (description "" (string))
  (subcommands '() (list))
  (options '() (list))
  (args '() (list))
  (order 0 (integer))
  (groups '() (list))
  ;;(runner 'undefined (lambda)))
  (runner 'undefined (any)))

(defrecord option
  (name 'undefined (atom))
  (short 0 (integer))
  (long "" (string))
  (help "" (string))
  (type 'undefined (atom))
  (default 'undefined (any))
  (required 'false (atom))
  (multi-valued 'false (atom))
  (delim "," (string))
  ;;(value-parser 'undefined (lambda))
  (value-parser 'undefined (any))
  (env "" (string))
  (order 0 (integer))
  (groups '() (list)))

(defrecord arg
  (name "" (string))
  (required 'false (atom))
  (help "" (string)))

(defrecord help
  (title-heading "NAME" (string))
  (title "" (string))
  (synopsis-heading "SYNOPSIS" (string))
  (synopsis "" (string))
  (description-heading "DESCRIPTION" (string))
  (description "" (string))
  (options-heading "OPTIONS" (string))
  (options "" (string))
  (additional "" (string)))

;; For compat with Erlang getopt ... maybe not neccesary?

(defrecord spec
  (name 'undefined (atom))
  (short 0 (integer))
  (long "" (string))
  (arg '() (list))
  (help "" (string)))

(defun --loaded-lcli-records-- ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).

  This function needs to be the last one in this include."
  'ok)
