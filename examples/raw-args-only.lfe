#!/usr/bin/env lfe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; This script shows 1) the default argument/option handling from Erlang
;;;; that is most useful for LFE scripts, 2) the two helper variables
;;;; provided by LFE for getting arguments and script name, and 3) the same
;;;; functionality as provided by the lcli library.
;;;;
;;;; To run the script, ensure that lfe is in your $PATH, then:
;;;;
;;;;   $ ./examples/simple.lfe --greeting "Hello" --greetee "World"
;;;;

(include-lib "lcli/include/records.lfe")

(defun main ()
  (let ((pas (init:get_plain_arguments))
        ((match-plain-args script s args as) (lcli:args)))
    (lfe_io:format "Plain args: ~p~n" `(,pas))
    (lfe_io:format "Helper script name: ~p~n" `(,script-name))
    (lfe_io:format "Helper script args: ~p~n" `(,script-args))
    (lfe_io:format "Script name: ~p~n" `(,s))
    (lfe_io:format "Script args: ~p~n" `(,as))))

(main)
