#!/usr/bin/env lfe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; This script simply shows the default argument/option handling from
;;;; Erlang that is most useful for LFE scripts. It shows nothing of the
;;;; functionality provided by lcli.
;;;;
;;;; To run the script, ensure that lfe is in your $PATH, then:
;;;;
;;;;   $ ./examples/init-only.lfe --greeting "Hello" --greetee "World"
;;;;

(defun main ()
  (let ((args (init:get_plain_arguments)))
    (lfe_io:format "Args: ~p~n" `(,args))))

(main)
