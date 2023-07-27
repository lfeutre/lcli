(defrecord app
  (name "app" (string))
  (title "" (string))
  (desc "" (string))
  (options '() (list))
  (args '() (list))
  (commands '() (list))
  ;;(with-colour 'true (atom))) --> move to lib-opts
  (lib-opts '() (list)))

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
  (desc-heading "DESCRIPTION" (string))
  (desc "" (string))
  (options-heading "OPTIONS" (string))
  (options "" (string))
  (commands-heading "COMMANDS" (string))
  (commands "" (string))
  (additional "" (string)))

(defrecord parsed
  (app "" (string))
  (commands '() (list))
  (options #m() (any))
  (args '() (list)))

(defun --loaded-lcli-records-- ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).

  This function needs to be the last one in this include."
  'ok)
