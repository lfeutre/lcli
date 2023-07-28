#!/usr/bin/env lfe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; This script extracts individual parsed (and default) arguements after
;;;; they have been checked. The functionality demonstrated below is of the
;;;; thin wrapping functions around the getopt Erlang library.
;;;;
;;;; To run the script, ensure that lfe is in your $PATH, then:
;;;;
;;;;   $ ./examples/simple-parse1.lfe
;;;;   Hello, World!
;;;;
;;;;   $ ./examples/simple-parse1.lfe -g "Awwww, "
;;;;   Awwww, World!
;;;;
;;;;   $ ./examples/simple-parse1.lfe -e 'Mr. Bill!'
;;;;   Hello, Mr. Bill!
;;;;
;;;;   $ ./examples/simple-parse1.lfe -g "Awwww, " -e 'Nuts!'
;;;;   Awwww, Nuts!
;;;;
;;;;   $ ./examples/simple-parse1.lfe --greeting 'On, no! ' -e 'Nuts!'
;;;;   On, no! Nuts!
;;;;
;;;;   $ ./examples/simple-parse1.lfe --greeting 'On, no! ' --greetee 'Mr. Bill!'
;;;;   On, no! Mr. Bill!
;;;;

(include-lib "lcli/include/records.lfe")

(defun options ()
  `(#m(type option name help short #\h long "help"
       help "Display this help text.")
    #m(type option name greeting short #\g long "greeting" val-type string default "Hello, "
       help "A greeting for someone.")
    #m(type option name greetee short #\e long "greetee" val-type string default "World!"
       help "Someone or something to greet.")))

(defun main ()
  (case (lcli:parse (options))
    ((match-parsed options opts)
     (lfe_io:format "~p~n" `(,opts))
     (lcli:usage (options))
     (halt 0))
    (result
     ;;(lfe_io:format "~p~n" `(,result))
     (halt 0))))

(main)