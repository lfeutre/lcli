(defmodule lcli
  (export all))

(defun parse (specs)
  (parse specs (lcli-args:get-raw-args)))

(defun parse
  ((specs `(#(script ,script) #(args ,args)))
    (if (lcli-cmds:has-commands? specs)
      (parse-with-commands script args specs)
      (parse-without-commands script args specs))))

(defun parse-with-commands (script args specs)
  (let* ((opts-only-spec (lcli-opts:filter-specs specs))
       (`#(,opts ,args) (lcli-opts:parse-opts opts-only-spec args))
       (commands (lcli-opts:get-opt 'commands specs)))
  `(#(cmd ,script)
    #(opts ,opts)
    #(args ,args)
    #(cmds ,(parse-commands commands)))))

(defun parse-without-commands (script args specs)
  (let ((`#(,opts ,args) (lcli-opts:parse-opts specs args)))
    `(#(cmd ,script)
      #(opts ,opts)
      #(args ,args)
      #(cmds undefined))))

(defun parse-command
  ((`#(,cmd #(opts ,opts)))
    `#(,cmd #(opts ,(parse opts))))
  ((command)
    (logjam:start)
    (logjam:error "Couldn't parse: ~p" `(,command))
    (timer:sleep 1000)
    command))

(defun parse-commands (commands)
  (lists:map #'parse-command/1 commands))

(defun command-usage (commands)
  "Print usage information for the defined commands."
  (lfe_io:format "Commands:~n" '()))

(defun usage (specs)
  "Wrap the ``(getopt:usage)`` function, providing the computed script name."
  (usage specs (lcli-args:get-script)))

(defun usage (specs script)
  "Wrap the ``(getopt:usage)`` function while providing support for command
  usage."
  (getopt:usage specs script)
  (if (lcli-cmds:has-commands? specs)
    (command-usage (lcli-opts:get-opt 'commands specs))))

(defun get-spec (key specs)
  "Given the key for a spec and a list of specs, return the first spec that
  has the given key.

  Note that this function returns the whole spec (key and value) not just the
  value."
  (case (lists:keyfind key 1 specs)
    (spec spec)
    ('false 'undefined)))
