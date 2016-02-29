#!/usr/bin/env lfe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; This script extracts individual parsed (and default) arguements after
;;;; they have been checked.
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
(defun opt-spec ()
  `(#(greeting #\g "greeting" #(string "Hello, ") "A greeting for someone.")
    #(greetee #\e "greetee" #(string "World!") "Someone or something to greet.")))

(defun main ()
  (case (lcli:parse (opt-spec))
    (`(,_ #(opts ,opts) ,_)
      (lfe_io:format "~s~s~n"
                     `(,(proplists:get_value 'greeting opts)
                       ,(proplists:get_value 'greetee opts))))
    (result
      (error result))))

(main)
