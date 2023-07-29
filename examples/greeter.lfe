#!/usr/bin/env lfe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; This script extracts individual parsed (and default) arguements after
;;;; they have been checked. The functionality demonstrated below is of the
;;;; thin wrapping functions around the getopt Erlang library.
;;;;
;;;; To run the script, ensure that lfe is in your $PATH, then:
;;;;
;;;;   $ ./examples/greeter.lfe
;;;;   Hello, World!
;;;;
;;;;   $ ./examples/greeter.lfe -g "Awwww, "
;;;;   Awwww, World!
;;;;
;;;;   $ ./examples/greeter.lfe -e 'Mr. Bill!'
;;;;   Hello, Mr. Bill!
;;;;
;;;;   $ ./examples/greeter.lfe -g "Awwww, " -e 'Nuts!'
;;;;   Awwww, Nuts!
;;;;
;;;;   $ ./examples/greeter.lfe --greeting 'On, no! ' -e 'Nuts!'
;;;;   On, no! Nuts!
;;;;
;;;;   $ ./examples/greeter.lfe --greeting 'On, no! ' --greetee 'Mr. Bill!!'
;;;;   On, no! Mr. Bill!!
;;;;

(include-lib "lcli/include/records.lfe")

(defun app ()
  `#m(type app
      name "greeter"
      title "Greeting generator"
      desc "An lcli example app that uses options to generate greetings."
      options (
        #m(type option name help short #\h long "help"
           help "Display this help text.")
        #m(type option name greeting short #\g long "greeting" val-type string default "Hello, "
           help "A greeting for someone.")
        #m(type option name greetee short #\e long "greetee" val-type string default "World!"
           help "Someone or something to greet."))))

(defun main ()
  (let ((opts (app)))
    (case (lcli:parse opts)
      ((match-parsed options `#m(help true))
       (lcli:usage opts)
       (halt 0))
      ((match-parsed options opts error '())
       (lfe_io:format "~s~s~n"
                      (list (mref opts 'greeting)
                            (mref opts 'greetee)))
       (halt 0))
      ((match-parsed error err)
       (io:format "~n~s~n~n" (list err))))))

(main)
