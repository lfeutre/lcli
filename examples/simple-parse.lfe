#!/usr/bin/env lfe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; This script shows parsed arguements after they have been checked.
;;;;
;;;; To run the script, ensure that lfe is in your $PATH, then:
;;;;
;;;;   $ ./examples/simple-parse.lfe first --greeting "Hello" second --greetee "World" third
;;;;
(defun get-spec ()
  `(#(greeting #\g "greeting" #(string "Hello, ") "A greeting for someone.")
    #(greetee #\e "greetee" #(string "World!") "Someone or something to greet.")))

(defun main ()
  (case (lcli:parse (get-spec))
    (`(#(cmd ,script) #(opts ,opts) #(args ,args))
      (lfe_io:format "Script name: ~p~n" `(,script))
      (lfe_io:format "Script options: ~p~n" `(,opts))
      (lfe_io:format "Script args: ~p~n" `(,args)))
    (result
      (logjam:start)
      (logjam:error "Unexpected result: ~p" `(,result)))))

(main)
