#!/usr/bin/env lfe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; This script extracts individual parsed (and default) arguements after
;;;; they have been checked. The functionality demonstrated below is of the
;;;; thin wrapping functions around the getopt Erlang library.
;;;;
;;;; To run the script, ensure that lfe is in your $PATH, then:
;;;;
;;;;   $ ./examples/simple-parse.lfe
;;;;   Hello, World!
;;;;
;;;;   $ ./examples/simple-parse.lfe -g "Awwww, "
;;;;   Awwww, World!
;;;;
;;;;   $ ./examples/simple-parse.lfe -e "Mr. Bill!"
;;;;   Hello, Mr. Bill!
;;;;
;;;;   $ ./examples/simple-parse.lfe -g "Awwww, " -e "Nuts!"
;;;;   Awwww, Nuts!
;;;;
;;;;   $ ./examples/simple-parse.lfe --greeting "On, no! " -e "Nuts!"
;;;;   On, no! Nuts!
;;;;
;;;;   $ ./examples/simple-parse.lfe --greeting "On, no! " --greetee "Mr. Bill!"
;;;;   On, no! Mr. Bill!
;;;;
;;;; To see the help output:
;;;;
;;;;   $ ./examples/simple-parse.lfe --help
;;;;   Usage: ./examples/simple-parse.lfe [-h] [-g [<greeting>]] [-e [<greetee>]]
;;;;
;;;;     -h, --help      Display this help text.
;;;;     -g, --greeting  A greeting for someone. [default: Hello, ]
;;;;     -e, --greetee   Someone or something to greet. [default: World!]
;;;;
(defun opt-spec ()
  `(#(help #\h "help" undefined "Display this help text.")
    #(greeting #\g "greeting" #(string "Hello, ") "A greeting for someone.")
    #(greetee #\e "greetee" #(string "World!") "Someone or something to greet.")))

(defun help ()
  (lcli:usage (opt-spec)))

(defun help (msg)
  (lfe_io:format "~n~s~n~n" `(,msg))
  (help))

(defun handle-options (opts)
  (cond
    ((== opts 'opt-parse-error)
      (help "Error: could not parse given option(s)")
      (halt 1))
    ((lcli:help? opts)
      (help)
      (halt 0))
    ('true
      (lfe_io:format "~s~s~n"
                    `(,(lcli:get-opt 'greeting opts)
                      ,(lcli:get-opt 'greetee opts)))
      (halt 0))))

(defun main ()
  (let ((`(,_ #(opts ,opts) ,_) (lcli:parse (opt-spec))))
    (handle-options opts)))

(main)
