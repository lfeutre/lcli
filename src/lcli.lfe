(defmodule lcli
  (export all))

(include-lib "logjam/include/logjam.hrl")
(include-lib "lcli/include/records.lfe")

(defun start ()
  (logjam:set-dev-config))

(defun parse (specs)
  (parse specs (lcli-args:get-raw)))

;; TODO: change 2nd arg to map
(defun parse
  ((specs `(#(script ,script) #(args ,args)))
   (let ((cmds (lcli-cmds:get specs)))
     (if (== (length cmds) 0)
       (parse-without-commands specs script args)
       (parse-with-commands specs cmds script args)))))

(defun parse-with-commands (specs cmds script args)
  (let* ((opts-only-spec (lcli-cmds:filter specs))
         (`#m(opts ,opts args ,args) (lcli-opts:parse opts-only-spec args)))
    (make-parsed
     app script
     commands cmds
     options opts
     args args)))

(defun parse-without-commands (specs script args)
  (let ((`#m(opts ,opts args ,args) (lcli-opts:parse specs args)))
    (make-parsed
     app script
     options opts
     args args)))

(defun parse-command
  ((`#(,cmd #(opts ,opts)))
   `#(,cmd #(opts ,(parse opts))))
  ((command)
   (log-error "Couldn't parse: ~p" `(,command))
   (timer:sleep 250)
   command))

(defun parse-commands (commands)
  (lists:map #'parse-command/1 commands))

(defun command-usage (commands)
  "Print usage information for the defined commands."
  (lfe_io:format "Commands:~n" '()))

(defun usage (specs)
  "Wrap the `(getopt:usage)` function, providing the computed script name."
  (usage specs (lcli-args:get-script)))

(defun usage (specs script)
  "Wrap the `(getopt:usage)` function while providing support for command
  usage."
  (lcli-opts:usage specs script)
  (if (lcli-cmds:commands? specs)
    (command-usage (lcli-opts:get 'commands specs))))

(defun get-spec (key specs)
  "Given the key for a spec and a list of specs, return the first spec that
  has the given key.

  Note that this function returns the whole spec (key and value) not just the
  value."
  (case (lists:keyfind key 1 specs)
    (spec spec)
    ('false 'undefined)))

;;; Metadata

(defun version ()
  (lcli-vsn:get))

(defun versions ()
  (lcli-vsn:all))
