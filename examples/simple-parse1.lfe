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
;;;;   $ ./examples/simple-parse1.lfe -e "Mr. Bill!"
;;;;   Hello, Mr. Bill!
;;;;
;;;;   $ ./examples/simple-parse1.lfe -g "Awwww, " -e "Nuts!"
;;;;   Awwww, Nuts!
;;;;
;;;;   $ ./examples/simple-parse1.lfe --greeting "On, no! " -e "Nuts!"
;;;;   On, no! Nuts!
;;;;
;;;;   $ ./examples/simple-parse1.lfe --greeting "On, no! " --greetee "Mr. Bill!"
;;;;   On, no! Mr. Bill!
;;;;
(defun opt-spec ()
  `(#(greeting #\g "greeting" #(string "Hello, ") "A greeting for someone.")
    #(greetee #\e "greetee" #(string "World!") "Someone or something to greet.")))

(defun main ()
  (case (lcli:parse (opt-spec))
    (`(,_ #(opts ,opts) ,_)
      (io:format "~s~s~n"
                 `(,(lcli:get-opt 'greeting opts)
                   ,(lcli:get-opt 'greetee opts)))
      (halt 0))
    (result
      (error result)
      (halt 1))))

(main)
